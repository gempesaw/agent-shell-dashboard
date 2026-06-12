;;; agent-shell-dashboard-tracking.el --- Opt-in tracking (advice, hooks, summary capture) -*- lexical-binding: t; -*-

;;; Commentary:

;; The advice/hook layer that enriches dashboard rows with summaries,
;; last-prompt metadata, and active-session persistence.  None of
;; this fires unless the user calls `agent-shell-dashboard-setup'.

;;; Code:

(require 'dash)
(require 'ht)
(require 's)
(require 'agent-shell-dashboard-data)

(defvar-local agent-shell-dashboard--prompt-count 0
  "Number of user prompts sent in this agent-shell session.
Maintained by submission-tracking advice; rolls over the summary
capture every `agent-shell-dashboard-summary-interval' prompts.")

(defvar-local agent-shell-dashboard--summary-pending nil
  "Non-nil if a summary request was just queued and we're waiting
for the agent's response so we can parse it out of the buffer.")

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

(defun agent-shell-dashboard--track-prompt-submission (orig-fun &rest args)
  "Advice around `shell-maker-submit' to track prompts and capture last prompt."
  (when (derived-mode-p 'agent-shell-mode)
    (let ((input (s-trim (buffer-substring-no-properties
                          (shell-maker--prompt-end-position) (point-max)))))
      (unless (s-blank? input)
        (if (s-equals? input agent-shell-dashboard-summary-prompt)
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
  (or (s-blank? line)
      (s-starts-with? "▶" line)
      (s-equals? line "Done")
      (s-starts-with? "Requesting " line)
      (s-starts-with? "Creating " line)
      (s-starts-with? "Subscribing" line)
      (s-starts-with? "Initializing" line)
      (s-equals? line "Ready")
      (s-starts-with? "<shell-maker" line)))

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
                (let ((line (s-trim (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position)))))
                  (unless (agent-shell-dashboard--status-line-p line)
                    (setq found-summary line)))
                (forward-line -1))
              (when found-summary
                (setq agent-shell-dashboard--summary-pending nil)
                ;; Store the full captured line.  Column rendering
                ;; truncates for display; search reads :summary in
                ;; full so longer tag-lists stay queryable.
                (setq agent-shell-dashboard--buffer-summary found-summary)
                (when-let* ((sid (map-nested-elt agent-shell--state '(:session :id))))
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
             (let ((trimmed (s-trim request)))
               (and (not (s-blank? trimmed))
                    (not (s-equals? trimmed agent-shell-dashboard-summary-prompt)))))
    (setq-local agent-shell-dashboard--buffer-last-prompt-text (s-trim request))
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
  (--keep (unless (eq it exclude-buffer)
            (agent-shell-dashboard--buffer-session-data it))
          (agent-shell-dashboard--all-buffers)))

(defun agent-shell-dashboard-save-active-sessions (&optional exclude-buffer)
  "Write active agent-shell session metadata to `agent-shell-dashboard-active-sessions-file'.
EXCLUDE-BUFFER, when non-nil, is omitted (e.g. a buffer being killed)."
  (interactive)
  (let ((sessions (agent-shell-dashboard--collect-active-sessions exclude-buffer)))
    (with-temp-file agent-shell-dashboard-active-sessions-file
      (let ((print-length nil)
            (print-level nil))
        (insert ";; -*- mode: lisp-data; -*-\n")
        (insert ";; Auto-generated by agent-shell-dashboard. Do not edit.\n")
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
  (when (and session-id summary (not (s-blank? summary)))
    (let* ((existing (or (agent-shell-dashboard--read-summary-archive) '()))
           (without (--remove (equal (plist-get it :session-id) session-id)
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
  (let ((map (ht-create)))
    (dolist (entry (or (ignore-errors (agent-shell-dashboard--read-summary-archive))
                       '()))
      (when-let* ((id (plist-get entry :session-id))
                  (s (plist-get entry :summary)))
        (ht-set! map id s)))
    (dolist (entry (or (ignore-errors (agent-shell-dashboard--read-active-sessions))
                       '()))
      (when-let* ((id (plist-get entry :session-id))
                  (s (plist-get entry :summary)))
        (ht-set! map id s)))
    (dolist (buf (agent-shell-dashboard--all-buffers))
      (with-current-buffer buf
        (when-let* ((id (map-nested-elt agent-shell--state '(:session :id)))
                    (s agent-shell-dashboard--buffer-summary))
          (ht-set! map id s))))
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
             (s (and id (ht-get summaries id))))
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
         (existing-ids (--keep (with-current-buffer it
                                 (map-nested-elt agent-shell--state '(:session :id)))
                               (agent-shell-dashboard--all-buffers)))
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

(provide 'agent-shell-dashboard-tracking)
;;; agent-shell-dashboard-tracking.el ends here
