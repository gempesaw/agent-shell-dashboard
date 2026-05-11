;;; agent-shell-dashboard.el --- Magit-style dashboard for agent-shell sessions -*- lexical-binding: t; -*-

;; Author: Daniel Gempesaw <gempesaw@modular.com>
;; URL: https://github.com/gempesaw/agent-shell-dashboard
;; Package-Requires: ((emacs "28.1") (agent-shell "0.1.0"))
;; Keywords: tools, convenience

;;; Commentary:

;; A magit-style dashboard listing agent-shell sessions, grouped by
;; project and laid out as side-by-side columns.  Live, awaiting,
;; working, and closed sessions are all surfaced; per-row commands
;; let you visit, fork, kill, send a prompt, or jump between
;; columns via single-key overlays.
;;
;; The dashboard does not track per-session enrichment data on its
;; own.  Buffer-local variables `agent-shell-dashboard--buffer-summary',
;; `agent-shell-dashboard--buffer-last-prompt-time', and
;; `agent-shell-dashboard--buffer-last-prompt-text' can be populated by
;; external code (advice on `shell-maker-submit', agent-shell event
;; subscriptions, etc).  When unset, the dashboard degrades gracefully:
;; no summary, no awaiting-state, no preview line.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'agent-shell)

(defvar agent-shell-dashboard-buffer-name "*agent-shell-dashboard*"
  "Name of the agent-shell dashboard buffer.")

(defvar-local agent-shell-dashboard--saved-window-config nil
  "Window configuration saved when the dashboard was opened.")

(defvar-local agent-shell-dashboard--row-positions nil
  "Buffer positions of rendered rows, in display order.")

(defvar-local agent-shell-dashboard--columns nil
  "Per-column metadata: list of plists with :header-pos and :first-row-pos.")

(defvar-local agent-shell-dashboard--highlight-overlays nil
  "Overlays highlighting the row at point, refreshed in `post-command-hook'.")

(defvar-local agent-shell-dashboard--row-status-overlays nil
  "Persistent overlays applied to working / permission rows on render.")

(defvar-local agent-shell-dashboard--prompt-count 0
  "Number of user prompts sent in this agent-shell session.
Maintained by submission-tracking advice; rolls over the summary
capture every `agent-shell-dashboard-summary-interval' prompts.")

(defvar-local agent-shell-dashboard--summary-pending nil
  "Non-nil if a summary request was just queued and we're waiting
for the agent's response so we can parse it out of the buffer.")

(defface agent-shell-dashboard-working-row-face
  '((((background dark))  :background "#1f2d3d")
    (((background light)) :background "#e7eef6"))
  "Persistent row background for sessions currently working."
  :group 'dg-agent-shell)

(defface agent-shell-dashboard-permission-row-face
  '((((background dark))  :background "#3a2a1a")
    (((background light)) :background "#f4ece0"))
  "Persistent row background for sessions awaiting a permission decision."
  :group 'dg-agent-shell)

(defface agent-shell-dashboard-awaiting-row-face
  '((((background dark))  :background "#5e5028")
    (((background light)) :background "#fef9b8"))
  "Persistent row background for sessions whose agent finished
recently and are waiting for the user's next prompt.
Gold-tinted to read as `your turn' on the fairyfloss palette."
  :group 'dg-agent-shell)

(defcustom agent-shell-dashboard-awaiting-minutes 15
  "Promote a ready session to the `awaiting' status when its last
prompt was submitted within this many minutes."
  :type 'number
  :group 'dg-agent-shell)

(defcustom agent-shell-dashboard-jump-keys
  "asdfjkl;qwertyuiopzxcvbnm"
  "Letters used as single-key shortcuts in `agent-shell-dashboard-jump'."
  :type 'string
  :group 'agent-shell-dashboard)

(defcustom agent-shell-dashboard-active-sessions-file
  (expand-file-name "agent-shell-dashboard-active.el" user-emacs-directory)
  "Path to the file that holds a snapshot of live sessions.
Maintained automatically once `agent-shell-dashboard-setup' is
called: rewritten on `kill-emacs-hook' (whole snapshot) and on
`kill-buffer-hook' (the dying buffer omitted)."
  :type 'file
  :group 'agent-shell-dashboard)

(defcustom agent-shell-dashboard-claude-projects-dir
  (expand-file-name "~/.claude/projects")
  "Directory where Claude Code stores per-project session JSONL logs.
The dashboard enumerates `*.jsonl' files under each subdirectory
of this path to populate closed-session rows.  This is the default
data source when no tracking file is configured.  Set to nil to
skip filesystem enumeration entirely."
  :type '(choice (const :tag "Skip JSONL scan" nil) directory)
  :group 'agent-shell-dashboard)

(defcustom agent-shell-dashboard-summary-archive-file
  (expand-file-name "agent-shell-dashboard-summaries.el" user-emacs-directory)
  "Append-only archive of captured session summaries.
Survives session close so summary text remains searchable
indefinitely.  Written by `agent-shell-dashboard--archive-summary'
when the summary capture succeeds."
  :type 'file
  :group 'agent-shell-dashboard)

(defcustom agent-shell-dashboard-summary-prompt
  "Reply with ONLY: a 3-6 word summary of what we're working on. No other text."
  "Prompt injected into each agent-shell session to capture a summary.
The first response after this prompt is parsed out of the buffer
and stored as the session summary."
  :type 'string
  :group 'agent-shell-dashboard)

(defcustom agent-shell-dashboard-summary-interval 10
  "Number of user prompts between automatic summary captures.
The first capture fires after the very first prompt; subsequent
captures fire when the prompt count is divisible by this value."
  :type 'integer
  :group 'agent-shell-dashboard)

(defcustom agent-shell-dashboard-identifier-to-config-fn-alist
  '((claude-code . agent-shell-anthropic-make-claude-code-config))
  "Map an agent's `:identifier' symbol to its config-builder function.
Used when resuming a session into a new buffer."
  :type '(alist :key-type symbol :value-type function)
  :group 'agent-shell-dashboard)

(defvar-local agent-shell-dashboard--buffer-summary nil
  "Per-buffer session summary read by the dashboard for display.
Populated by external tracking code (e.g. submission advice).")

(defvar-local agent-shell-dashboard--buffer-last-prompt-time nil
  "Time of the most recently submitted user prompt in this buffer.
Drives the `awaiting' status promotion and the activity column.")

(defvar-local agent-shell-dashboard--buffer-last-prompt-text nil
  "Text of the most recently submitted user prompt in this buffer.
Shown as the preview line beneath each row's metadata line.")


;;; Inlined helpers (formerly namespaced under the personal config).

(defun agent-shell-dashboard--all-buffers ()
  "Return all live agent-shell-mode buffers."
  (seq-filter (lambda (buf)
                (with-current-buffer buf
                  (derived-mode-p 'agent-shell-mode)))
              (agent-shell-buffers)))

(defun agent-shell-dashboard--format-relative-time (time)
  "Format TIME relative to now, e.g. `5m ago', `2h ago'."
  (when time
    (let ((seconds (float-time (time-subtract (current-time) time))))
      (cond
       ((< seconds 60)    (format "%ds ago"  (truncate seconds)))
       ((< seconds 3600)  (format "%dm ago"  (truncate (/ seconds 60))))
       ((< seconds 86400) (format "%dh ago"  (truncate (/ seconds 3600))))
       (t                 (format "%dd ago"  (truncate (/ seconds 86400))))))))

(defun agent-shell-dashboard--buffer-status (buf)
  "Return BUF's session activity: `working' or `ready'.
Permission-state is not tracked by the public package; override
this if your environment can detect a pending permission request."
  (cond
   ((map-elt (buffer-local-value 'agent-shell--state buf) :active-requests)
    'working)
   (t 'ready)))

(defun agent-shell-dashboard--buffer-session-data (buf)
  "Return persistable data for BUF, or nil if it has no session ID."
  (with-current-buffer buf
    (when-let* ((session-id (map-nested-elt agent-shell--state '(:session :id)))
                (config (map-elt agent-shell--state :agent-config))
                (identifier (map-elt config :identifier)))
      (list :buffer-name (buffer-name)
            :session-id session-id
            :cwd default-directory
            :identifier identifier
            :summary agent-shell-dashboard--buffer-summary))))

(defun agent-shell-dashboard--resume-session (entry)
  "Open a new agent-shell buffer resuming the session described by ENTRY.
ENTRY is a plist with :session-id :cwd :identifier :summary keys.
Returns the new buffer, or nil if it could not be located."
  (let* ((session-id (plist-get entry :session-id))
         (cwd (plist-get entry :cwd))
         (identifier (plist-get entry :identifier))
         (summary (plist-get entry :summary))
         (config-fn (alist-get identifier
                               agent-shell-dashboard-identifier-to-config-fn-alist)))
    (unless session-id (user-error "No session id"))
    (unless (and config-fn (fboundp config-fn))
      (user-error "No config builder registered for %S" identifier))
    (unless (and cwd (file-directory-p cwd))
      (user-error "cwd no longer exists: %s" cwd))
    (let ((before (agent-shell-dashboard--all-buffers)))
      (let* ((default-directory cwd)
             (agent-shell-cwd-function (lambda () cwd)))
        (agent-shell-start :config (funcall config-fn) :session-id session-id))
      (let ((new-buffer (car (seq-difference (agent-shell-dashboard--all-buffers)
                                             before))))
        (when (and new-buffer summary)
          (with-current-buffer new-buffer
            (setq-local agent-shell-dashboard--buffer-summary summary)))
        new-buffer))))

(defun agent-shell-dashboard--read-active-sessions ()
  "Read `agent-shell-dashboard-active-sessions-file' or return nil."
  (when (and agent-shell-dashboard-active-sessions-file
             (file-exists-p agent-shell-dashboard-active-sessions-file))
    (with-temp-buffer
      (insert-file-contents agent-shell-dashboard-active-sessions-file)
      (goto-char (point-min))
      (condition-case nil (read (current-buffer)) (error nil)))))

(defun agent-shell-dashboard--read-summary-archive ()
  "Read `agent-shell-dashboard-summary-archive-file' or return nil."
  (when (and agent-shell-dashboard-summary-archive-file
             (file-exists-p agent-shell-dashboard-summary-archive-file))
    (with-temp-buffer
      (insert-file-contents agent-shell-dashboard-summary-archive-file)
      (goto-char (point-min))
      (condition-case nil (read (current-buffer)) (error nil)))))

(defun agent-shell-dashboard--write-active-sessions (entries)
  "Overwrite `agent-shell-dashboard-active-sessions-file' with ENTRIES."
  (when agent-shell-dashboard-active-sessions-file
    (with-temp-file agent-shell-dashboard-active-sessions-file
      (let ((print-length nil)
            (print-level nil))
        (insert ";; -*- mode: lisp-data; -*-\n")
        (prin1 entries (current-buffer))
        (insert "\n")))))

(defun agent-shell-dashboard--write-summary-archive (entries)
  "Overwrite `agent-shell-dashboard-summary-archive-file' with ENTRIES."
  (when agent-shell-dashboard-summary-archive-file
    (with-temp-file agent-shell-dashboard-summary-archive-file
      (let ((print-length nil)
            (print-level nil))
        (insert ";; -*- mode: lisp-data; -*-\n")
        (insert ";; Append-only archive of agent-shell session summaries.\n")
        (prin1 entries (current-buffer))
        (insert "\n")))))

(defun agent-shell-dashboard--sniff-jsonl-cwd (path)
  "Extract the cwd recorded in JSONL file PATH, if any.
Reads up to the first 8KB looking for a JSON object with a `cwd'
field.  Returns nil if not found."
  (when (file-readable-p path)
    (with-temp-buffer
      (insert-file-contents path nil 0 8192)
      (goto-char (point-min))
      (let ((cwd nil))
        (while (and (not cwd) (not (eobp)))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (when (string-match "\"cwd\":\"\\([^\"]+\\)\"" line)
              (setq cwd (match-string 1 line))))
          (forward-line 1))
        cwd))))

(defun agent-shell-dashboard--enumerate-jsonls ()
  "Return session entries enumerated from Claude's JSONL logs.
Each entry is a plist with :session-id :cwd :updated-at :identifier.
Returns nil if `agent-shell-dashboard-claude-projects-dir' is nil
or does not exist.  cwd is sniffed once per project directory by
reading the first JSONL found there."
  (when (and agent-shell-dashboard-claude-projects-dir
             (file-directory-p agent-shell-dashboard-claude-projects-dir))
    (let ((entries nil))
      (dolist (proj-dir (directory-files
                         agent-shell-dashboard-claude-projects-dir t
                         directory-files-no-dot-files-regexp))
        (when (file-directory-p proj-dir)
          (let* ((jsonls (directory-files proj-dir t "\\.jsonl\\'"))
                 (cwd (and jsonls
                           (agent-shell-dashboard--sniff-jsonl-cwd
                            (car jsonls)))))
            (when cwd
              (dolist (file jsonls)
                (let ((basename (file-name-base file))
                      (mtime (file-attribute-modification-time
                              (file-attributes file))))
                  (push (list :session-id basename
                              :cwd cwd
                              :updated-at (format-time-string "%FT%T%z" mtime)
                              :identifier 'claude-code
                              :summary nil)
                        entries)))))))
      entries)))

(defun agent-shell-dashboard-start-new-session ()
  "Start a new agent-shell session using the first registered config."
  (interactive)
  (let* ((entry (car agent-shell-dashboard-identifier-to-config-fn-alist))
         (config-fn (cdr entry)))
    (unless config-fn
      (user-error "No agent config registered in %s"
                  'agent-shell-dashboard-identifier-to-config-fn-alist))
    (agent-shell-start :config (funcall config-fn))))

(defun agent-shell-dashboard-start-new-session-pick-repo ()
  "Pick a project root, then start a new session there."
  (interactive)
  (let* ((dir (cond
               ((and (boundp 'projectile-known-projects)
                     projectile-known-projects)
                (completing-read "Project: " projectile-known-projects nil t))
               (t (read-directory-name "Project: "))))
         (default-directory (file-name-as-directory (expand-file-name dir)))
         (agent-shell-cwd-function (lambda () default-directory)))
    (agent-shell-dashboard-start-new-session)))

(defun agent-shell-dashboard--track-prompt-submission (orig-fun &rest args)
  "Advice around `shell-maker-submit' to track prompts and capture last prompt."
  (when (derived-mode-p 'agent-shell-mode)
    (let ((input (string-trim (buffer-substring-no-properties
                               (shell-maker--prompt-end-position) (point-max)))))
      (unless (string-empty-p input)
        (if (string= input agent-shell-dashboard-summary-prompt)
            (setq agent-shell-dashboard--summary-pending t)
          (setq agent-shell-dashboard--buffer-last-prompt-text input)
          (setq agent-shell-dashboard--buffer-last-prompt-time (current-time))
          (cl-incf agent-shell-dashboard--prompt-count)
          (agent-shell-dashboard--maybe-queue-summary)))))
  (apply orig-fun args))

(defun agent-shell-dashboard--poll-for-summary (buf attempts)
  "Poll BUF for summary response, up to ATTEMPTS times."
  (when (and (buffer-live-p buf) (> attempts 0))
    (with-current-buffer buf
      (if (and agent-shell-dashboard--summary-pending (not (shell-maker-busy)))
          (progn
            (agent-shell-dashboard--check-for-summary-capture)
            (when agent-shell-dashboard--summary-pending
              (run-with-timer 2 nil #'agent-shell-dashboard--poll-for-summary buf (1- attempts))))
        (when agent-shell-dashboard--summary-pending
          (run-with-timer 2 nil #'agent-shell-dashboard--poll-for-summary buf (1- attempts)))))))

(defun agent-shell-dashboard--maybe-queue-summary ()
  "Queue a summary request if conditions are met."
  (when (and (derived-mode-p 'agent-shell-mode)
             (not agent-shell-dashboard--summary-pending)
             (or (= agent-shell-dashboard--prompt-count 1)
                 (= 0 (mod agent-shell-dashboard--prompt-count agent-shell-dashboard-summary-interval))))
    (let ((buf (current-buffer)))
      (run-with-timer 1 nil
                      (lambda ()
                        (when (buffer-live-p buf)
                          (with-current-buffer buf
                            (agent-shell-queue-request agent-shell-dashboard-summary-prompt)
                            (run-with-timer 5 nil #'agent-shell-dashboard--poll-for-summary buf 10))))))))

(defun agent-shell-dashboard--status-line-p (line)
  "Return non-nil if LINE is an agent-shell status message to skip."
  (or (string-empty-p line)
      (string-prefix-p "▶" line)
      (string-equal line "Done")
      (string-prefix-p "Requesting " line)
      (string-prefix-p "Creating " line)
      (string-prefix-p "Subscribing" line)
      (string-prefix-p "Initializing" line)
      (string-equal line "Ready")
      (string-prefix-p "<shell-maker" line)))

(defun agent-shell-dashboard--check-for-summary-capture ()
  "Check if we should capture a summary from the buffer."
  (when agent-shell-dashboard--summary-pending
    (save-excursion
      (goto-char (point-max))
      (let* ((shell-prompt (or (map-nested-elt agent-shell--state '(:agent-config :shell-prompt))
                               "Claude> "))
             (prompt-line-re (concat "^" (regexp-quote shell-prompt)))
             (search-pattern (concat prompt-line-re (regexp-quote agent-shell-dashboard-summary-prompt))))
        (when (re-search-backward search-pattern nil t)
          (when (re-search-forward "<shell-maker-end-of-prompt>\n" nil t)
            (let* ((start (point))
                   (end (if (re-search-forward prompt-line-re nil t)
                            (match-beginning 0)
                          (point-max)))
                   (found-summary nil))
              (goto-char end)
              (forward-line -1)
              (while (and (>= (point) start) (not found-summary))
                (let ((line (string-trim (buffer-substring-no-properties
                                          (line-beginning-position)
                                          (line-end-position)))))
                  (unless (agent-shell-dashboard--status-line-p line)
                    (setq found-summary line)))
                (forward-line -1))
              (when found-summary
                (setq agent-shell-dashboard--summary-pending nil)
                (setq agent-shell-dashboard--buffer-summary
                      (truncate-string-to-width found-summary 60 nil nil "..."))
                (when-let ((sid (map-nested-elt agent-shell--state '(:session :id))))
                  (agent-shell-dashboard--archive-summary
                   sid agent-shell-dashboard--buffer-summary default-directory))
                (message "Summary for %s: %s" (buffer-name) agent-shell-dashboard--buffer-summary)))))))))

(defun agent-shell-dashboard--after-response-hook (orig-fun &rest args)
  "Advice to capture summary after any response completes."
  (prog1 (apply orig-fun args)
    (when (derived-mode-p 'agent-shell-mode)
      (agent-shell-dashboard--check-for-summary-capture))))

(defun agent-shell-dashboard--track-queue-request (orig-fun request &rest args)
  "Advice around `agent-shell-queue-request' to record submitted prompts."
  (when (and (derived-mode-p 'agent-shell-mode)
             (stringp request)
             (let ((trimmed (string-trim request)))
               (and (not (string-empty-p trimmed))
                    (not (string= trimmed agent-shell-dashboard-summary-prompt)))))
    (setq-local agent-shell-dashboard--buffer-last-prompt-text (string-trim request))
    (setq-local agent-shell-dashboard--buffer-last-prompt-time (current-time)))
  (apply orig-fun request args))

(defun agent-shell-dashboard--safe-clean-up (orig-fun &rest args)
  "Advice around `agent-shell--clean-up' to prevent errors from blocking buffer kill.
The upstream clean-up can fail with \"Cannot modify map in-place\" or
\"Text is read-only\", which prevents killing agent-shell buffers."
  (let ((inhibit-read-only t))
    (condition-case err
        (apply orig-fun args)
      (error (message "agent-shell clean-up error (ignored): %s" err)))))

(defun agent-shell-dashboard--collect-active-sessions (&optional exclude-buffer)
  "Collect persistable data for all live agent-shell buffers with session IDs.
EXCLUDE-BUFFER is omitted from the result if non-nil."
  (delq nil
        (mapcar (lambda (buf)
                  (unless (eq buf exclude-buffer)
                    (agent-shell-dashboard--buffer-session-data buf)))
                (agent-shell-dashboard--all-buffers))))

(defun agent-shell-dashboard-save-active-sessions (&optional exclude-buffer)
  "Write active agent-shell session metadata to `agent-shell-dashboard-active-sessions-file'.
EXCLUDE-BUFFER, when non-nil, is omitted (e.g. a buffer being killed)."
  (interactive)
  (let ((sessions (agent-shell-dashboard--collect-active-sessions exclude-buffer)))
    (with-temp-file agent-shell-dashboard-active-sessions-file
      (let ((print-length nil)
            (print-level nil))
        (insert ";; -*- mode: lisp-data; -*-\n")
        (insert ";; Auto-generated by dg-agent-shell. Do not edit.\n")
        (prin1 sessions (current-buffer))
        (insert "\n")))
    (when (called-interactively-p 'interactive)
      (message "Saved %d agent-shell session%s"
               (length sessions)
               (if (= 1 (length sessions)) "" "s")))))

(defun agent-shell-dashboard--save-on-buffer-kill ()
  "Re-save the session list when an agent-shell buffer is killed."
  (when (derived-mode-p 'agent-shell-mode)
    (let ((dying-buffer (current-buffer)))
      (run-at-time 0 nil
                   (lambda ()
                     (agent-shell-dashboard-save-active-sessions dying-buffer))))))

(defun agent-shell-dashboard--archive-summary (session-id summary &optional cwd)
  "Upsert SESSION-ID's SUMMARY in the archive, optionally with CWD context."
  (when (and session-id summary (not (string-empty-p summary)))
    (let* ((existing (or (agent-shell-dashboard--read-summary-archive) '()))
           (without (seq-remove (lambda (e)
                                  (equal (plist-get e :session-id) session-id))
                                existing))
           (project (and cwd (file-name-nondirectory
                              (directory-file-name cwd))))
           (entry (list :session-id session-id
                        :summary summary
                        :project (or project "")
                        :cwd (or cwd "")
                        :updated-at (format-time-string "%FT%T%z"))))
      (agent-shell-dashboard--write-summary-archive (cons entry without)))))

(defun agent-shell-dashboard--all-known-summaries ()
  "Return a hash table mapping session-id to our session summary.
Live buffers > active-sessions file > long-term summary archive."
  (let ((map (make-hash-table :test 'equal)))
    (dolist (entry (or (ignore-errors (agent-shell-dashboard--read-summary-archive))
                       '()))
      (when-let* ((id (plist-get entry :session-id))
                  (s (plist-get entry :summary)))
        (puthash id s map)))
    (dolist (entry (or (ignore-errors (agent-shell-dashboard--read-active-sessions))
                       '()))
      (when-let* ((id (plist-get entry :session-id))
                  (s (plist-get entry :summary)))
        (puthash id s map)))
    (dolist (buf (agent-shell-dashboard--all-buffers))
      (with-current-buffer buf
        (when-let* ((id (map-nested-elt agent-shell--state '(:session :id)))
                    (s agent-shell-dashboard--buffer-summary))
          (puthash id s map))))
    map))

(defun agent-shell-dashboard--session-selection-columns-advice (cols)
  "Append `summary' column to COLS for the session-selection prompt."
  (append cols '(summary)))

(defun agent-shell-dashboard--session-column-value-advice (orig-fun column acp-session)
  "Provide value for our `summary' COLUMN; delegate to ORIG-FUN otherwise.
ACP-SESSION is the alist describing the session being labelled."
  (if (eq column 'summary)
      (let* ((id (map-elt acp-session 'sessionId))
             (summaries (agent-shell-dashboard--all-known-summaries))
             (s (and id (gethash id summaries))))
        (or s ""))
    (funcall orig-fun column acp-session)))

(defun agent-shell-dashboard--session-column-face-advice (orig-fun column)
  "Face for our `summary' COLUMN; delegate to ORIG-FUN otherwise."
  (if (eq column 'summary)
      'font-lock-doc-face
    (funcall orig-fun column)))

(defun agent-shell-dashboard-restore-active-sessions ()
  "Restore agent-shell sessions saved in `agent-shell-dashboard-active-sessions-file'.
Each session is reopened via ACP session resume/load, replaying
its conversation history.  Skips sessions already open and ones
whose cwd no longer exists."
  (interactive)
  (let* ((sessions (agent-shell-dashboard--read-active-sessions))
         (existing-ids (delq nil
                             (mapcar (lambda (buf)
                                       (with-current-buffer buf
                                         (map-nested-elt agent-shell--state '(:session :id))))
                                     (agent-shell-dashboard--all-buffers))))
         (restored 0)
         (skipped 0))
    (dolist (entry sessions)
      (let* ((session-id (plist-get entry :session-id))
             (cwd (plist-get entry :cwd))
             (summary (plist-get entry :summary))
             (identifier (plist-get entry :identifier))
             (config-fn (alist-get identifier agent-shell-dashboard-identifier-to-config-fn-alist)))
        (cond
         ((member session-id existing-ids)
          (cl-incf skipped))
         ((not (and config-fn (fboundp config-fn)))
          (message "agent-shell: no config builder for %S, skipping %s"
                   identifier session-id)
          (cl-incf skipped))
         ((not (and cwd (file-directory-p cwd)))
          (message "agent-shell: cwd %s no longer exists, skipping" cwd)
          (cl-incf skipped))
         (t
          (let* ((default-directory cwd)
                 (agent-shell-cwd-function (lambda () cwd))
                 (config (funcall config-fn)))
            (agent-shell-start :config config :session-id session-id)
            (when summary
              (dolist (buf (agent-shell-dashboard--all-buffers))
                (with-current-buffer buf
                  (when (and (not agent-shell-dashboard--buffer-summary)
                             (equal session-id
                                    (map-nested-elt agent-shell--state
                                                    '(:session :id))))
                    (setq-local agent-shell-dashboard--buffer-summary summary))))))
          (cl-incf restored)))))
    (message "agent-shell: restored %d session%s, skipped %d"
             restored (if (= 1 restored) "" "s") skipped)))

(defvar agent-shell-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g")   #'agent-shell-dashboard-refresh)
    (define-key map (kbd "n")   #'agent-shell-dashboard-next)
    (define-key map (kbd "p")   #'agent-shell-dashboard-previous)
    (define-key map (kbd "q")   #'agent-shell-dashboard-quit)
    (define-key map (kbd "RET") #'agent-shell-dashboard-visit)
    (define-key map (kbd "o")   #'agent-shell-dashboard-visit-other-window)
    (define-key map (kbd "f")   #'agent-shell-dashboard-fork)
    (define-key map (kbd "k")   #'agent-shell-dashboard-kill)
    (define-key map (kbd "N")   #'agent-shell-dashboard-start-new-session)
    (define-key map (kbd "M")   #'agent-shell-dashboard-start-new-session-pick-repo)
    (define-key map (kbd "j")   #'agent-shell-dashboard-jump)
    (define-key map (kbd "r")   #'agent-shell-dashboard-reload)
    map)
  "Keymap for `agent-shell-dashboard-mode'.")

(define-derived-mode agent-shell-dashboard-mode special-mode "AgentDash"
  "Magit-style dashboard for live and closed agent-shell sessions."
  (setq truncate-lines t)
  (setq-local revert-buffer-function
              (lambda (&rest _) (agent-shell-dashboard-refresh)))
  (add-hook 'post-command-hook
            #'agent-shell-dashboard--update-highlight nil t))

(defun agent-shell-dashboard--clear-highlight ()
  "Remove the row-highlight overlays."
  (mapc #'delete-overlay agent-shell-dashboard--highlight-overlays)
  (setq agent-shell-dashboard--highlight-overlays nil))

(defun agent-shell-dashboard--apply-row-status-overlays ()
  "Tint working / permission rows with a persistent background.
Cursor's hl-line overlay has higher priority and so wins on the
row at point."
  (mapc #'delete-overlay agent-shell-dashboard--row-status-overlays)
  (setq agent-shell-dashboard--row-status-overlays nil)
  (let ((pos (point-min))
        (max (point-max)))
    (while (< pos max)
      (let* ((next (or (next-single-property-change pos 'dg-row) max))
             (row (get-text-property pos 'dg-row))
             (face (and row
                        (pcase (plist-get row :status)
                          ('working    'agent-shell-dashboard-working-row-face)
                          ('permission 'agent-shell-dashboard-permission-row-face)
                          ('awaiting   'agent-shell-dashboard-awaiting-row-face)))))
        (when face
          (let ((ov (make-overlay pos next)))
            (overlay-put ov 'face face)
            (overlay-put ov 'priority -100)
            (push ov agent-shell-dashboard--row-status-overlays)))
        (setq pos next)))))

(defun agent-shell-dashboard--update-highlight ()
  "Highlight just the cell at point — i.e., the single line of the
row that the cursor is currently on. The other line of the same
row stays untouched so its persistent status background remains
visible (working / permission / awaiting tints)."
  (agent-shell-dashboard--clear-highlight)
  (when-let ((row (get-text-property (point) 'dg-row)))
    (let* ((end (or (next-single-property-change (point) 'dg-row) (point-max)))
           (start (let ((p (point)))
                    (while (and (> p (point-min))
                                (eq (get-text-property (1- p) 'dg-row) row))
                      (setq p (1- p)))
                    p))
           (ov (make-overlay start end)))
      (overlay-put ov 'face 'hl-line)
      (overlay-put ov 'priority -50)
      (push ov agent-shell-dashboard--highlight-overlays))))

(defun agent-shell-dashboard--row-from-buffer (buf)
  "Build a dashboard row plist from live agent-shell BUF, or nil."
  (with-current-buffer buf
    (when-let ((id (map-nested-elt agent-shell--state '(:session :id))))
      (let* ((raw-status (agent-shell-dashboard--buffer-status buf))
             (last-time agent-shell-dashboard--buffer-last-prompt-time)
             (mins-since (and last-time
                              (/ (float-time
                                  (time-subtract (current-time) last-time))
                                 60.0)))
             (status (if (and (eq raw-status 'ready)
                              mins-since
                              (< mins-since
                                 agent-shell-dashboard-awaiting-minutes))
                         'awaiting
                       raw-status)))
        (list :session-id id
              :buffer buf
              :cwd default-directory
              :identifier (map-elt (map-elt agent-shell--state :agent-config)
                                   :identifier)
              :summary agent-shell-dashboard--buffer-summary
              :status status
              :last-prompt-time agent-shell-dashboard--buffer-last-prompt-time
              :last-prompt-text agent-shell-dashboard--buffer-last-prompt-text)))))

(defun agent-shell-dashboard--rows ()
  "Build the list of dashboard rows, deduped by session id.
Sources, in priority order:
  1. live agent-shell buffers
  2. `agent-shell-dashboard-active-sessions-file' (if set)
  3. `agent-shell-dashboard-summary-archive-file' (if set)
  4. JSONL logs under `agent-shell-dashboard-claude-projects-dir'."
  (let ((seen (make-hash-table :test 'equal))
        (rows nil))
    (dolist (buf (agent-shell-dashboard--all-buffers))
      (when-let ((row (agent-shell-dashboard--row-from-buffer buf)))
        (puthash (plist-get row :session-id) t seen)
        (push row rows)))
    (dolist (entry (or (ignore-errors (agent-shell-dashboard--read-active-sessions)) '()))
      (when-let ((id (plist-get entry :session-id))
                 ((not (gethash id seen))))
        (puthash id t seen)
        (push (list :session-id id
                    :buffer nil
                    :cwd (plist-get entry :cwd)
                    :identifier (plist-get entry :identifier)
                    :summary (plist-get entry :summary)
                    :status 'closed
                    :updated-at nil
                    :last-prompt-time nil
                    :last-prompt-text nil)
              rows)))
    (let ((archive (or (ignore-errors (agent-shell-dashboard--read-summary-archive)) '())))
      (dolist (entry archive)
        (when-let ((id (plist-get entry :session-id))
                   ((not (gethash id seen))))
          (puthash id t seen)
          (push (list :session-id id
                      :buffer nil
                      :cwd (plist-get entry :cwd)
                      ;; Archive entries predate per-agent identifiers.
                      :identifier 'claude-code
                      :summary (plist-get entry :summary)
                      :status 'closed
                      :updated-at (plist-get entry :updated-at)
                      :last-prompt-time nil
                      :last-prompt-text nil)
                rows)))
      (dolist (entry (agent-shell-dashboard--enumerate-jsonls))
        (when-let ((id (plist-get entry :session-id))
                   ((not (gethash id seen))))
          (puthash id t seen)
          (push (list :session-id id
                      :buffer nil
                      :cwd (plist-get entry :cwd)
                      :identifier (plist-get entry :identifier)
                      :summary nil
                      :status 'closed
                      :updated-at (plist-get entry :updated-at)
                      :last-prompt-time nil
                      :last-prompt-text nil)
                rows)))
      ;; Backfill missing summaries on rows from earlier sources.
      (dolist (row rows)
        (unless (plist-get row :summary)
          (when-let ((entry (seq-find
                             (lambda (e)
                               (equal (plist-get e :session-id)
                                      (plist-get row :session-id)))
                             archive)))
            (plist-put row :summary (plist-get entry :summary))))))
    (sort rows #'agent-shell-dashboard--row-less-p)))

(defun agent-shell-dashboard--row-time (row)
  "Return the most relevant timestamp for ROW, or nil."
  (or (plist-get row :last-prompt-time)
      (and (plist-get row :updated-at)
           (ignore-errors (date-to-time (plist-get row :updated-at))))))

(defun agent-shell-dashboard--row-less-p (a b)
  "Return non-nil if dashboard row A should sort before B.
Live ahead of closed; within each group, newest activity first."
  (let ((live-a (plist-get a :buffer))
        (live-b (plist-get b :buffer))
        (ta (agent-shell-dashboard--row-time a))
        (tb (agent-shell-dashboard--row-time b)))
    (cond
     ((and live-a (not live-b)) t)
     ((and (not live-a) live-b) nil)
     ((and ta tb) (time-less-p tb ta))
     (ta t)
     (tb nil)
     (t (string< (or (plist-get a :session-id) "")
                 (or (plist-get b :session-id) ""))))))

(defun agent-shell-dashboard--row-is-today (row)
  "Return non-nil if ROW's most-recent activity is on today's calendar date."
  (when-let ((time (agent-shell-dashboard--row-time row)))
    (string= (format-time-string "%Y-%m-%d" time)
             (format-time-string "%Y-%m-%d" (current-time)))))

(defun agent-shell-dashboard--status-glyph (status &optional is-today)
  "Return a propertized one-character glyph for row STATUS.
If IS-TODAY is non-nil, closed rows are rendered in a brighter face
to distinguish today's killed sessions from older archived ones."
  (pcase status
    ('ready      (propertize "●" 'face 'success))
    ('awaiting   (propertize "◉" 'face '(:inherit font-lock-variable-name-face
                                          :weight bold)))
    ('working    (propertize "▶" 'face '(:inherit font-lock-keyword-face
                                          :weight bold)))
    ('permission (propertize "!" 'face '(:inherit warning :weight bold)))
    ('closed     (propertize "○" 'face (if is-today
                                            'font-lock-string-face
                                          'shadow)))
    (_           (propertize "·" 'face 'shadow))))

(defun agent-shell-dashboard--row-project (row)
  "Return the project basename for ROW's cwd, or empty string."
  (or (and (plist-get row :cwd)
           (file-name-nondirectory
            (directory-file-name (plist-get row :cwd))))
      ""))

(defcustom agent-shell-dashboard-column-width 80
  "Width in characters of each project column in the dashboard."
  :type 'integer
  :group 'dg-agent-shell)

(defcustom agent-shell-dashboard-column-gap 2
  "Number of blank chars between adjacent project columns."
  :type 'integer
  :group 'dg-agent-shell)

(defcustom agent-shell-dashboard-pinned-projects '("infra")
  "Project names rendered as dedicated full-height leftmost columns.
Pinned columns appear in this list's order, left to right.  All
remaining projects flow into right-side column bands.  An empty
list reverts to the uniform banded layout across all projects."
  :type '(repeat string)
  :group 'dg-agent-shell)

(defun agent-shell-dashboard--row-cells (row width)
  "Return two cells (line1 line2) representing ROW, each WIDTH-wide.
Each cell is a plist with :string and :row keys."
  (let* ((status (plist-get row :status))
         (is-today (agent-shell-dashboard--row-is-today row))
         (glyph (agent-shell-dashboard--status-glyph status is-today))
         (activity (or (agent-shell-dashboard--format-relative-time
                        (agent-shell-dashboard--row-time row))
                       (if (eq status 'closed) "closed" "—")))
         (summary (or (plist-get row :summary) ""))
         (preview (replace-regexp-in-string
                   "[ \t\n\r]+" " "
                   (or (plist-get row :last-prompt-text) "")))
         (line1 (format "  %s  %-7s  %s"
                        glyph
                        (propertize activity 'face 'shadow)
                        summary))
         (line2 (concat "        "
                        (propertize "> " 'face 'shadow)
                        (propertize (if (string-empty-p preview) "—" preview)
                                    'face 'shadow))))
    (list (list :string (truncate-string-to-width line1 width nil ?\s)
                :row row
                :row-start t)
          (list :string (truncate-string-to-width line2 width nil ?\s)
                :row row))))

(defun agent-shell-dashboard--group-cells (group width)
  "Return list of cells for GROUP rendered in a WIDTH-char column.
Each cell is a plist with :string and optional :row."
  (let* ((project (car group))
         (rows (cdr group))
         (n-live (seq-count (lambda (r) (plist-get r :buffer)) rows))
         (n-closed (- (length rows) n-live))
         (header (concat
                  (propertize (if (string-empty-p project) "(none)" project)
                              'face (if (zerop n-live)
                                        '(:inherit shadow :height 1.4)
                                      '(:inherit font-lock-function-name-face
                                        :height 1.4 :weight bold)))
                  "  "
                  (propertize (format "(%d live, %d closed)" n-live n-closed)
                              'face 'shadow)))
         (cells (list (list :string (truncate-string-to-width header width nil ?\s))
                      (list :string (make-string width ?\s)))))
    (dolist (row rows)
      (setq cells (append cells (agent-shell-dashboard--row-cells row width))))
    cells))

(defun agent-shell-dashboard--group-rows (rows)
  "Group ROWS by project. Returns alist of (PROJECT . ROWS).
Sections are sorted live-first, then by most recent activity."
  (let ((groups nil))
    (dolist (row rows)
      (let* ((project (or (agent-shell-dashboard--row-project row) ""))
             (cell (assoc project groups)))
        (if cell
            (setcdr cell (cons row (cdr cell)))
          (push (cons project (list row)) groups))))
    (dolist (cell groups)
      (setcdr cell (nreverse (cdr cell))))
    (sort groups #'agent-shell-dashboard--group-less-p)))

(defun agent-shell-dashboard--group-newest-time (rows)
  "Return the most recent activity time across ROWS, or nil."
  (car (sort (delq nil (mapcar #'agent-shell-dashboard--row-time rows))
             (lambda (x y) (time-less-p y x)))))

(defun agent-shell-dashboard--group-less-p (a b)
  "Sort group A before B by liveness, then most-recent activity, then name."
  (let* ((rows-a (cdr a))
         (rows-b (cdr b))
         (live-a (seq-some (lambda (r) (plist-get r :buffer)) rows-a))
         (live-b (seq-some (lambda (r) (plist-get r :buffer)) rows-b))
         (ta (agent-shell-dashboard--group-newest-time rows-a))
         (tb (agent-shell-dashboard--group-newest-time rows-b)))
    (cond
     ((and live-a (not live-b)) t)
     ((and (not live-a) live-b) nil)
     ((and ta tb) (time-less-p tb ta))
     (ta t)
     (tb nil)
     (t (string< (car a) (car b))))))

(defun agent-shell-dashboard--insert-cell (cell)
  "Insert CELL at point, stamping `dg-row' and recording first-row-pos.
Returns the buffer position where the cell starts."
  (let ((cell-start (point))
        (str (plist-get cell :string))
        (row (plist-get cell :row)))
    (insert str)
    (when row
      (when (plist-get cell :row-start)
        (push cell-start agent-shell-dashboard--row-positions))
      (add-text-properties cell-start (point) (list 'dg-row row)))
    cell-start))

(defun agent-shell-dashboard--insert-column-band (groups width gap)
  "Insert a band of side-by-side GROUPS, each WIDTH chars wide, GAP between.
Records each column's header-pos and first-row-pos in
`agent-shell-dashboard--columns'."
  (let* ((columns (mapcar (lambda (g) (agent-shell-dashboard--group-cells g width))
                          groups))
         (height (apply #'max (mapcar #'length columns)))
         (blank (list :string (make-string width ?\s)))
         (gap-str (make-string gap ?\s))
         (last-col-idx (1- (length columns)))
         (header-positions (make-vector (length columns) nil))
         (first-row-positions (make-vector (length columns) nil)))
    (dotimes (line-idx height)
      (let ((col-idx 0))
        (dolist (col columns)
          (let* ((cell (or (nth line-idx col) blank))
                 (str (plist-get cell :string))
                 (row (plist-get cell :row))
                 (cell-start (point)))
            (when (= line-idx 0)
              (aset header-positions col-idx cell-start))
            (insert str)
            (when row
              (when (plist-get cell :row-start)
                (push cell-start agent-shell-dashboard--row-positions)
                (unless (aref first-row-positions col-idx)
                  (aset first-row-positions col-idx cell-start)))
              (add-text-properties cell-start (point) (list 'dg-row row))))
          (when (< col-idx last-col-idx)
            (insert gap-str))
          (cl-incf col-idx)))
      (insert "\n"))
    (insert "\n")
    (dotimes (col-idx (length columns))
      (push (list :project (car (nth col-idx groups))
                  :header-pos (aref header-positions col-idx)
                  :first-row-pos (aref first-row-positions col-idx))
            agent-shell-dashboard--columns))))

(defun agent-shell-dashboard--insert-asymmetric (pinned-groups right-groups width gap)
  "Render PINNED-GROUPS as dedicated full-height left columns.
RIGHT-GROUPS are laid out in bands to the right of the pinned ones.
Column metadata is recorded in `agent-shell-dashboard--columns'
in left-to-right order: pinned first, then right-side band-by-band."
  (let* ((gap-str (make-string gap ?\s))
         (blank-cell (list :string (make-string width ?\s)))
         (pinned-columns (mapcar (lambda (g) (agent-shell-dashboard--group-cells g width))
                                 pinned-groups))
         (n-pinned (length pinned-columns))
         (left-block-width (if (zerop n-pinned)
                               0
                             (+ (* width n-pinned) (* gap n-pinned))))
         (right-area-width (max 80 (- (max 80 (frame-width)) left-block-width)))
         (cols-per-band (max 1 (/ right-area-width (+ width gap))))
         (right-bands (seq-partition right-groups cols-per-band))
         (right-lines nil)
         (band-idx 0))
    (dolist (band right-bands)
      (let* ((cols (mapcar (lambda (g) (agent-shell-dashboard--group-cells g width))
                           band))
             (band-h (apply #'max (mapcar #'length cols))))
        (dotimes (line-idx band-h)
          (push (list :type 'data
                      :band-idx band-idx
                      :line-in-band line-idx
                      :cells (mapcar (lambda (col) (or (nth line-idx col) blank-cell))
                                     cols))
                right-lines))
        (push (list :type 'spacer) right-lines))
      (cl-incf band-idx))
    (setq right-lines (nreverse right-lines))
    (let* ((max-pinned-h (if (zerop n-pinned)
                             0
                           (apply #'max (mapcar #'length pinned-columns))))
           (total (max max-pinned-h (length right-lines)))
           ;; Per-pinned-column tracking: vectors indexed by pinned col idx.
           (pinned-headers (make-vector n-pinned nil))
           (pinned-firsts  (make-vector n-pinned nil))
           ;; Right-side tracking, keyed by (band-idx . col-idx).
           (right-tracker (make-hash-table :test 'equal)))
      (dotimes (line-idx total)
        ;; Pinned columns, left to right.
        (let ((p-idx 0))
          (dolist (col pinned-columns)
            (let* ((cell (or (nth line-idx col) blank-cell))
                   (cell-start (agent-shell-dashboard--insert-cell cell)))
              (when (= line-idx 0)
                (aset pinned-headers p-idx cell-start))
              (when (and (plist-get cell :row-start)
                         (not (aref pinned-firsts p-idx)))
                (aset pinned-firsts p-idx cell-start)))
            (insert gap-str)
            (cl-incf p-idx)))
        ;; Right-side cells (one band's worth, per line-idx).
        (let ((right-line (nth line-idx right-lines)))
          (when (and right-line (eq (plist-get right-line :type) 'data))
            (let ((b-idx (plist-get right-line :band-idx))
                  (line-in-band (plist-get right-line :line-in-band))
                  (cells (plist-get right-line :cells))
                  (col-idx 0)
                  (last-col (1- (length (plist-get right-line :cells)))))
              (dolist (cell cells)
                (let ((right-cell-start (agent-shell-dashboard--insert-cell cell))
                      (key (cons b-idx col-idx)))
                  (let ((entry (gethash key right-tracker)))
                    (when (= line-in-band 0)
                      (puthash key (cons right-cell-start (cdr entry)) right-tracker))
                    (when (and (plist-get cell :row-start)
                               (or (not entry) (not (cdr entry))))
                      (puthash key
                               (cons (or (car (gethash key right-tracker))
                                         right-cell-start)
                                     right-cell-start)
                               right-tracker))))
                (when (< col-idx last-col)
                  (insert gap-str))
                (cl-incf col-idx)))))
        (insert "\n"))
      ;; Pinned columns come first in jump/column order.
      (dotimes (p-idx n-pinned)
        (push (list :project (car (nth p-idx pinned-groups))
                    :header-pos (aref pinned-headers p-idx)
                    :first-row-pos (aref pinned-firsts p-idx))
              agent-shell-dashboard--columns))
      ;; Then right-side columns, band-by-band, col-by-col.
      (dotimes (b (length right-bands))
        (dotimes (c (length (nth b right-bands)))
          (let ((entry (gethash (cons b c) right-tracker)))
            (push (list :project (car (nth c (nth b right-bands)))
                        :header-pos (car entry)
                        :first-row-pos (cdr entry))
                  agent-shell-dashboard--columns)))))))

(defun agent-shell-dashboard-refresh ()
  "Re-render the dashboard contents, preserving the row at point.
Projects are laid out as side-by-side columns; if more projects
exist than fit horizontally, extra projects wrap to a second band."
  (interactive)
  (let* ((inhibit-read-only t)
         (saved-id (and (eq major-mode 'agent-shell-dashboard-mode)
                        (plist-get (get-text-property (point) 'dg-row)
                                   :session-id)))
         (rows (agent-shell-dashboard--rows))
         (live (seq-filter (lambda (r) (plist-get r :buffer)) rows))
         (closed (seq-remove (lambda (r) (plist-get r :buffer)) rows))
         (groups (agent-shell-dashboard--group-rows rows))
         (width agent-shell-dashboard-column-width)
         (gap agent-shell-dashboard-column-gap)
         (pinned-names agent-shell-dashboard-pinned-projects)
         ;; Resolve pinned project names to groups, preserving the requested order.
         (pinned-groups (delq nil (mapcar (lambda (n) (assoc n groups)) pinned-names)))
         (effective-groups
          (if pinned-groups
              (seq-remove (lambda (g) (memq g pinned-groups)) groups)
            groups))
         (cols-per-band (max 1 (/ (max 80 (frame-width)) (+ width gap))))
         (bands (seq-partition effective-groups cols-per-band)))
    (erase-buffer)
    (setq agent-shell-dashboard--row-positions nil)
    (setq agent-shell-dashboard--columns nil)
    (insert (propertize
             (format "Agent Shell  —  %d live, %d closed across %d project%s\n\n"
                     (length live) (length closed) (length groups)
                     (if (= 1 (length groups)) "" "s"))
             'face 'shadow))
    (cond
     (pinned-groups
      (agent-shell-dashboard--insert-asymmetric
       pinned-groups effective-groups width gap))
     (t
      (dolist (band bands)
        (agent-shell-dashboard--insert-column-band band width gap))))
    (setq agent-shell-dashboard--row-positions
          (sort agent-shell-dashboard--row-positions #'<))
    (setq agent-shell-dashboard--columns
          (nreverse agent-shell-dashboard--columns))
    (agent-shell-dashboard--apply-row-status-overlays)
    (cond
     ((and saved-id
           (cl-loop for pos in agent-shell-dashboard--row-positions
                    for r = (get-text-property pos 'dg-row)
                    when (equal saved-id (plist-get r :session-id))
                    return (progn (goto-char pos) t))))
     (agent-shell-dashboard--row-positions
      (goto-char (car agent-shell-dashboard--row-positions)))
     (t (goto-char (point-min))))))

(defun agent-shell-dashboard--column-slot (pos)
  "Return the horizontal column slot index for buffer POS."
  (save-excursion
    (goto-char pos)
    (/ (current-column)
       (+ agent-shell-dashboard-column-width
          agent-shell-dashboard-column-gap))))

(defun agent-shell-dashboard-next ()
  "Move to the next dashboard row in the same column."
  (interactive)
  (let* ((cur-slot (agent-shell-dashboard--column-slot (point)))
         (next (seq-find (lambda (pos)
                           (and (> pos (point))
                                (= cur-slot
                                   (agent-shell-dashboard--column-slot pos))))
                         agent-shell-dashboard--row-positions)))
    (when next (goto-char next))))

(defun agent-shell-dashboard-previous ()
  "Move to the previous dashboard row in the same column."
  (interactive)
  (let* ((cur-slot (agent-shell-dashboard--column-slot (point)))
         (prev (cl-loop for pos in (reverse agent-shell-dashboard--row-positions)
                        when (and (< pos (point))
                                  (= cur-slot
                                     (agent-shell-dashboard--column-slot pos)))
                        return pos)))
    (when prev (goto-char prev))))

(defun agent-shell-dashboard--row-at-point ()
  "Return the row plist at point, or signal a `user-error'."
  (or (get-text-property (point) 'dg-row)
      (user-error "No dashboard row at point")))

(defun agent-shell-dashboard--ensure-buffer (row)
  "Return a live buffer for ROW, resuming the session if it is closed."
  (let ((buf (plist-get row :buffer)))
    (if (and buf (buffer-live-p buf))
        buf
      (or (agent-shell-dashboard--resume-session row)
          (user-error "Could not resume session")))))

(defun agent-shell-dashboard-visit ()
  "Switch to the row's buffer (resuming first if it is closed)."
  (interactive)
  (let* ((row (agent-shell-dashboard--row-at-point))
         (buf (agent-shell-dashboard--ensure-buffer row)))
    (pop-to-buffer-same-window buf)))

(defun agent-shell-dashboard-visit-other-window ()
  "Display the row's buffer in another window, keeping focus here."
  (interactive)
  (let* ((row (agent-shell-dashboard--row-at-point))
         (buf (agent-shell-dashboard--ensure-buffer row)))
    (display-buffer buf)))

(defun agent-shell-dashboard-reload ()
  "Hard-reload the row's session in place.
Kills the current buffer (if any) and reopens a fresh one
resuming the same session id under the same cwd.  Use when the
agent has finished but the buffer didn't return control —
basically a fork-with-the-same-id-and-close-the-stale-one."
  (interactive)
  (let* ((row (agent-shell-dashboard--row-at-point))
         (session-id (or (plist-get row :session-id)
                         (user-error "Row has no session id")))
         (buf (plist-get row :buffer))
         (entry (list :session-id session-id
                      :cwd (plist-get row :cwd)
                      :identifier (or (plist-get row :identifier) 'claude-code)
                      :summary (plist-get row :summary))))
    (when (and buf (buffer-live-p buf))
      (let ((windows (get-buffer-window-list buf nil nil)))
        (kill-buffer buf)
        (dolist (w windows)
          (when (window-live-p w)
            (ignore-errors (delete-window w))))))
    (let ((new-buf (agent-shell-dashboard--resume-session entry)))
      (when new-buf
        (message "Reloaded session into %s" (buffer-name new-buf))))
    (agent-shell-dashboard-refresh)))

(defun agent-shell-dashboard-fork ()
  "Fork the row's session into a new shell."
  (interactive)
  (let* ((row (agent-shell-dashboard--row-at-point))
         (data (or (and (plist-get row :buffer)
                        (agent-shell-dashboard--buffer-session-data
                         (plist-get row :buffer)))
                   row))
         (buf (agent-shell-dashboard--resume-session data)))
    (when buf
      (message "Forked into %s" (buffer-name buf)))
    (agent-shell-dashboard-refresh)))

(defun agent-shell-dashboard--forget-session (session-id)
  "Remove SESSION-ID from the active-sessions file and summary archive,
when those file paths are configured.  Does not touch the underlying
agent's on-disk session log."
  (when agent-shell-dashboard-active-sessions-file
    (let* ((active (or (ignore-errors
                        (agent-shell-dashboard--read-active-sessions))
                       '()))
           (filtered (seq-remove
                      (lambda (e) (equal (plist-get e :session-id) session-id))
                      active)))
      (agent-shell-dashboard--write-active-sessions filtered)))
  (when agent-shell-dashboard-summary-archive-file
    (let* ((archive (or (ignore-errors
                         (agent-shell-dashboard--read-summary-archive))
                        '()))
           (filtered (seq-remove
                      (lambda (e) (equal (plist-get e :session-id) session-id))
                      archive)))
      (agent-shell-dashboard--write-summary-archive filtered))))

(defun agent-shell-dashboard-kill ()
  "Kill or forget the row at point depending on its state.
On a live row: kill the buffer and any windows displaying it.
On a closed row: permanently forget the session, removing it
from the active-sessions file and summary archive."
  (interactive)
  (let* ((row (agent-shell-dashboard--row-at-point))
         (buf (plist-get row :buffer))
         (session-id (plist-get row :session-id))
         (label (or (plist-get row :summary)
                    (and session-id
                         (substring session-id 0 (min 8 (length session-id))))
                    "this row")))
    (cond
     ((and buf (buffer-live-p buf))
      (when (yes-or-no-p (format "Kill %s? " (buffer-name buf)))
        (let ((windows (get-buffer-window-list buf nil nil)))
          (kill-buffer buf)
          (dolist (w windows)
            (when (window-live-p w)
              (ignore-errors (delete-window w)))))
        (agent-shell-dashboard-refresh)))
     ((null session-id)
      (user-error "Row has no session id"))
     (t
      (when (yes-or-no-p
             (format "Forget closed session \"%s\" permanently? " label))
        (agent-shell-dashboard--forget-session session-id)
        (agent-shell-dashboard-refresh))))))

(defun agent-shell-dashboard--render-jump-view (labeled)
  "Erase buffer and render only project headers with giant labels.
Letters are placed at the same horizontal column as the dashboard
showed each project, so the eye lands in the right place."
  (let* ((inhibit-read-only t)
         (descriptors (mapcar #'cdr labeled))
         (lines (make-hash-table :test 'eql)))
    ;; Collect (line-num . line-string) per relevant line, before erasing.
    (dolist (pair labeled)
      (let* ((ch (car pair))
             (d (cdr pair))
             (project (plist-get d :project))
             (h-pos (plist-get d :header-pos))
             (fr-pos (plist-get d :first-row-pos)))
        (when h-pos
          (let* ((line (line-number-at-pos h-pos))
                 (col (save-excursion (goto-char h-pos) (current-column)))
                 (existing (gethash line lines "")))
            (puthash line
                     (agent-shell-dashboard--place-at-col
                      existing col
                      (propertize (or project "")
                                  'face '(:inherit font-lock-function-name-face
                                          :height 1.5 :weight bold)))
                     lines)))
        (when fr-pos
          (let* ((line (line-number-at-pos fr-pos))
                 (col (save-excursion (goto-char fr-pos) (current-column)))
                 (existing (gethash line lines "")))
            (puthash line
                     (agent-shell-dashboard--place-at-col
                      existing col
                      (propertize (format " %c " ch)
                                  'face '(:foreground "black"
                                          :background "yellow"
                                          :weight bold
                                          :height 5.0)))
                     lines)))))
    (erase-buffer)
    (insert (propertize "Pick a column:\n\n" 'face 'shadow))
    (let* ((line-keys (sort (hash-table-keys lines) #'<))
           (max-line (or (car (last line-keys)) 0)))
      (dotimes (i max-line)
        (insert (gethash (1+ i) lines "") "\n")))))

(defun agent-shell-dashboard--place-at-col (existing col str)
  "Return EXISTING line with STR placed starting at column COL.
Pads with spaces if EXISTING is shorter than COL.  Overwrites
existing characters at and after COL."
  (let ((existing-w (string-width existing))
        (str-w (string-width str)))
    (cond
     ((>= col existing-w)
      (concat existing (make-string (- col existing-w) ?\s) str))
     (t
      (concat (substring existing 0 col)
              str
              (if (>= (+ col str-w) existing-w)
                  ""
                (substring existing (+ col str-w))))))))

(defun agent-shell-dashboard-jump ()
  "Jump to the first row of a column via single-key shortcuts.
Replaces the dashboard contents with a switch-window-style label
view while waiting for input, then restores the dashboard and
moves point to the chosen column's first row."
  (interactive)
  (let* ((targets (seq-filter (lambda (c) (plist-get c :first-row-pos))
                              agent-shell-dashboard--columns))
         (keys (string-to-list agent-shell-dashboard-jump-keys)))
    (when (null targets)
      (user-error "No columns to jump to"))
    (let* ((labeled (cl-loop for tgt in targets
                             for ch in keys
                             collect (cons ch tgt)))
           (chosen-idx nil)
           (saved-point (point)))
      (unwind-protect
          (progn
            (agent-shell-dashboard--render-jump-view labeled)
            (let* ((ch (read-char "Jump to column: "))
                   (idx (cl-position ch labeled :key #'car :test #'eq)))
              (cond
               ((or (eq ch ?\C-g) (eq ch 7)) nil)
               (idx (setq chosen-idx idx))
               (t (message "No such column: %c" ch)))))
        (agent-shell-dashboard-refresh)
        (cond
         (chosen-idx
          (let* ((new-targets (seq-filter (lambda (c) (plist-get c :first-row-pos))
                                          agent-shell-dashboard--columns))
                 (tgt (nth chosen-idx new-targets)))
            (when tgt (goto-char (plist-get tgt :first-row-pos)))))
         (t (goto-char (min saved-point (point-max)))))))))

(defun agent-shell-dashboard-quit ()
  "Bury the dashboard and restore the saved window configuration."
  (interactive)
  (let ((cfg agent-shell-dashboard--saved-window-config))
    (setq agent-shell-dashboard--saved-window-config nil)
    (quit-window)
    (when cfg (set-window-configuration cfg))))

(defun agent-shell-dashboard ()
  "Open the agent-shell dashboard, taking over the current frame.
The pre-existing window layout is restored when you press `q'.
Keys: RET visit, o open-in-other-window, n/p navigate, f fork,
k kill, s send prompt, N new session, M new session (pick repo),
g refresh, q quit and restore."
  (interactive)
  (let ((buf (get-buffer-create agent-shell-dashboard-buffer-name))
        (cfg (current-window-configuration)))
    (with-current-buffer buf
      (agent-shell-dashboard-mode)
      (setq-local agent-shell-dashboard--saved-window-config cfg)
      (agent-shell-dashboard-refresh))
    (pop-to-buffer-same-window buf)
    (delete-other-windows)))

(defun agent-shell-dashboard-setup ()
  "Install the dashboard's tracking machinery.
Adds advice on `shell-maker-submit', `agent-shell-queue-request',
`agent-shell--process-pending-request', `agent-shell--clean-up',
and the three session-picker hooks.  Adds `kill-emacs-hook' and
`kill-buffer-hook' for active-sessions persistence.

This is the function users should call from their init.el to
opt into automatic summary capture and active-session tracking."
  (interactive)
  (advice-add 'shell-maker-submit               :around #'agent-shell-dashboard--track-prompt-submission)
  (advice-add 'agent-shell-queue-request        :around #'agent-shell-dashboard--track-queue-request)
  (advice-add 'agent-shell--process-pending-request :around #'agent-shell-dashboard--after-response-hook)
  (advice-add 'agent-shell--clean-up            :around #'agent-shell-dashboard--safe-clean-up)
  (advice-add 'agent-shell--session-selection-columns :filter-return #'agent-shell-dashboard--session-selection-columns-advice)
  (advice-add 'agent-shell--session-column-value :around #'agent-shell-dashboard--session-column-value-advice)
  (advice-add 'agent-shell--session-column-face  :around #'agent-shell-dashboard--session-column-face-advice)
  (add-hook 'kill-emacs-hook  #'agent-shell-dashboard-save-active-sessions)
  (add-hook 'kill-buffer-hook #'agent-shell-dashboard--save-on-buffer-kill)
  (message "agent-shell-dashboard: tracking enabled"))

(defun agent-shell-dashboard-teardown ()
  "Remove the dashboard's tracking machinery installed by `setup'."
  (interactive)
  (advice-remove 'shell-maker-submit               #'agent-shell-dashboard--track-prompt-submission)
  (advice-remove 'agent-shell-queue-request        #'agent-shell-dashboard--track-queue-request)
  (advice-remove 'agent-shell--process-pending-request #'agent-shell-dashboard--after-response-hook)
  (advice-remove 'agent-shell--clean-up            #'agent-shell-dashboard--safe-clean-up)
  (advice-remove 'agent-shell--session-selection-columns #'agent-shell-dashboard--session-selection-columns-advice)
  (advice-remove 'agent-shell--session-column-value #'agent-shell-dashboard--session-column-value-advice)
  (advice-remove 'agent-shell--session-column-face  #'agent-shell-dashboard--session-column-face-advice)
  (remove-hook 'kill-emacs-hook  #'agent-shell-dashboard-save-active-sessions)
  (remove-hook 'kill-buffer-hook #'agent-shell-dashboard--save-on-buffer-kill)
  (message "agent-shell-dashboard: tracking disabled"))

(provide 'agent-shell-dashboard)
;;; agent-shell-dashboard.el ends here
