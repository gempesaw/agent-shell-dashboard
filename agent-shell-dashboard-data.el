;;; agent-shell-dashboard-data.el --- Data layer -*- lexical-binding: t; -*-

;;; Commentary:

;; Row sources, persistence (active-sessions snapshot, summary
;; archive, JSONL enumeration), per-buffer tracking state, session
;; resume helpers, and grouping/sorting primitives used by the
;; renderer.  This module is pure data and side-effect-free outside
;; of its own files; no UI or keymap concerns live here.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'agent-shell)

(defvar-local agent-shell-dashboard--buffer-summary nil
  "Per-buffer session summary read by the dashboard for display.
Populated by external tracking code (e.g. submission advice).")

(defvar-local agent-shell-dashboard--buffer-last-prompt-time nil
  "Time of the most recently submitted user prompt in this buffer.
Drives the `awaiting' status promotion and the activity column.")

(defvar-local agent-shell-dashboard--buffer-last-prompt-text nil
  "Text of the most recently submitted user prompt in this buffer.
Shown as the preview line beneath each row's metadata line.")

(defvar-local agent-shell-dashboard--buffer-marked-read-time nil
  "Time at which the user acknowledged the session's awaiting state.
A session in `awaiting' status remains so until either this is set
to a time newer than `--buffer-last-prompt-time' or a new prompt
is submitted.  Set via `agent-shell-dashboard-mark-as-read'.")

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

(defcustom agent-shell-dashboard-identifier-to-config-fn-alist
  '((claude-code . agent-shell-anthropic-make-claude-code-config))
  "Map an agent's `:identifier' symbol to its config-builder function.
Used when resuming a session into a new buffer."
  :type '(alist :key-type symbol :value-type function)
  :group 'agent-shell-dashboard)

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

(defun agent-shell-dashboard--row-from-buffer (buf)
  "Build a dashboard row plist from live agent-shell BUF, or nil."
  (with-current-buffer buf
    (when-let ((id (map-nested-elt agent-shell--state '(:session :id))))
      (let* ((raw-status (agent-shell-dashboard--buffer-status buf))
             (last-time agent-shell-dashboard--buffer-last-prompt-time)
             (marked-read agent-shell-dashboard--buffer-marked-read-time)
             (unread (and last-time
                          (or (not marked-read)
                              (time-less-p marked-read last-time))))
             (status (if (and (eq raw-status 'ready) unread)
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

(defun agent-shell-dashboard--row-project (row)
  "Return the project basename for ROW's cwd, or empty string."
  (or (and (plist-get row :cwd)
           (file-name-nondirectory
            (directory-file-name (plist-get row :cwd))))
      ""))

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

(defun agent-shell-dashboard--find-live-buffer-for-session (session-id)
  "Return the live agent-shell buffer whose session id is SESSION-ID, or nil."
  (when session-id
    (seq-find (lambda (buf)
                (with-current-buffer buf
                  (equal session-id
                         (map-nested-elt agent-shell--state '(:session :id)))))
              (agent-shell-dashboard--all-buffers))))

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

(provide 'agent-shell-dashboard-data)
;;; agent-shell-dashboard-data.el ends here
