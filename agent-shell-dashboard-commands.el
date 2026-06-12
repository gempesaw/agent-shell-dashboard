;;; agent-shell-dashboard-commands.el --- Mode, keymap, and interactive commands -*- lexical-binding: t; -*-

;;; Commentary:

;; The dashboard major mode, its keymap, and every interactive
;; command bound by it (navigate, visit, fork, kill, send, etc.),
;; plus the public `agent-shell-dashboard' entry point.

;;; Code:

(require 'dash)
(require 'agent-shell-dashboard-data)
(require 'agent-shell-dashboard-render)

(defvar agent-shell-dashboard-buffer-name "*agent-shell-dashboard*"
  "Name of the agent-shell dashboard buffer.")

(defvar-local agent-shell-dashboard--saved-window-config nil
  "Window configuration saved when the dashboard was opened.")

(defvar agent-shell-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g")   #'agent-shell-dashboard-refresh)
    (define-key map (kbd "n")   #'agent-shell-dashboard-next)
    (define-key map (kbd "p")   #'agent-shell-dashboard-previous)
    (define-key map (kbd "q")   #'agent-shell-dashboard-quit)
    (define-key map (kbd "RET") #'agent-shell-dashboard-visit)
    (define-key map (kbd "o")   #'agent-shell-dashboard-visit-other-window)
    (define-key map (kbd "f")   #'agent-shell-dashboard-fork)
    (define-key map (kbd "k")   #'agent-shell-dashboard-stop)
    (define-key map (kbd "K")   #'agent-shell-dashboard-forget)
    (define-key map (kbd "m")   #'agent-shell-dashboard-mark-as-read)
    (define-key map (kbd "s")   #'agent-shell-dashboard-send)
    (define-key map (kbd "N")   #'agent-shell-dashboard-start-new-session)
    (define-key map (kbd "M")   #'agent-shell-dashboard-start-new-session-pick-repo)
    (define-key map (kbd "j")   #'agent-shell-dashboard-jump)
    (define-key map (kbd "r")   #'agent-shell-dashboard-reload)
    (define-key map (kbd "/")   #'agent-shell-dashboard-search)
    map)
  "Keymap for `agent-shell-dashboard-mode'.")

(define-derived-mode agent-shell-dashboard-mode special-mode "AgentDash"
  "Magit-style dashboard for live and closed agent-shell sessions."
  (setq truncate-lines t)
  (setq-local revert-buffer-function
              (lambda (&rest _) (agent-shell-dashboard-refresh)))
  (add-hook 'post-command-hook
            #'agent-shell-dashboard--update-highlight nil t))

(defun agent-shell-dashboard-send ()
  "Send a prompt to the row's session via agent-shell's viewport.
Switches to the row's buffer (resuming a closed session if needed),
then pops the viewport compose window via `agent-shell-prompt-compose'.
Submit with `C-c C-c', cancel with `C-c C-k'."
  (interactive)
  (let* ((row (agent-shell-dashboard--row-at-point))
         (buf (agent-shell-dashboard--ensure-buffer row)))
    (pop-to-buffer buf)
    (call-interactively #'agent-shell-prompt-compose)))

(defun agent-shell-dashboard-start-new-session ()
  "Start a fresh agent-shell session using the first registered config.
When invoked from a dashboard row, the new session is spawned in
that row's project (cwd); otherwise it uses the current
`default-directory'.  Bypasses the session-resume picker; use `f'
on an existing row to fork or `r' to reload if you want to attach
to prior history."
  (interactive)
  (let* ((entry (car agent-shell-dashboard-identifier-to-config-fn-alist))
         (config-fn (cdr entry))
         (row (get-text-property (point) 'dg-row))
         (row-cwd (and row (plist-get row :cwd)))
         (agent-shell-session-strategy 'new))
    (unless config-fn
      (user-error "No agent config registered in %s"
                  'agent-shell-dashboard-identifier-to-config-fn-alist))
    (if (and row-cwd (file-directory-p row-cwd))
        (let* ((default-directory (file-name-as-directory
                                    (expand-file-name row-cwd)))
               (agent-shell-cwd-function (lambda () default-directory)))
          (agent-shell-start :config (funcall config-fn)))
      (agent-shell-start :config (funcall config-fn)))))

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

(defun agent-shell-dashboard-next ()
  "Move to the next dashboard row in the same column."
  (interactive)
  (let* ((cur-slot (agent-shell-dashboard--column-slot (point)))
         (next (--find (and (> it (point))
                            (= cur-slot
                               (agent-shell-dashboard--column-slot it)))
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

(defun agent-shell-dashboard-search ()
  "Jump to a row by searching project / summary / preview text.
Searches the full row data, not just what fits in the displayed
column.  Tags captured by `agent-shell-dashboard-setup' show up
here even when truncated off-screen."
  (interactive)
  (unless (eq major-mode 'agent-shell-dashboard-mode)
    (user-error "Not in agent-shell-dashboard-mode"))
  (unless agent-shell-dashboard--row-positions
    (user-error "No rows to search"))
  (let* ((candidates
          (--map
           (let* ((row (get-text-property it 'dg-row))
                  (cwd (plist-get row :cwd))
                  (project (or (and cwd (file-name-nondirectory
                                         (directory-file-name cwd)))
                               ""))
                  (summary (or (plist-get row :summary) ""))
                  (preview (or (plist-get row :last-prompt-text) ""))
                  (label (s-trim (format "%s | %s | %s"
                                         project summary preview))))
             (cons label it))
           agent-shell-dashboard--row-positions))
         (choice (completing-read "Jump to row: "
                                  (-map #'car candidates) nil t))
         (pos (cdr (assoc choice candidates))))
    (when pos (goto-char pos))))

(defun agent-shell-dashboard--row-at-point ()
  "Return the row plist at point, or signal a `user-error'."
  (or (get-text-property (point) 'dg-row)
      (user-error "No dashboard row at point")))

(defun agent-shell-dashboard--ensure-buffer (row)
  "Return a live buffer for ROW, resuming the session if needed.
Always rescans live buffers by session id first, so commands keep
working when the cached `:buffer' on the row is stale (e.g. the
session was resumed since the last dashboard refresh)."
  (let* ((cached (plist-get row :buffer))
         (sid (plist-get row :session-id)))
    (or (and (buffer-live-p cached) cached)
        (agent-shell-dashboard--find-live-buffer-for-session sid)
        (agent-shell-dashboard--resume-session row)
        (user-error "Could not resume session"))))

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

(defun agent-shell-dashboard-mark-as-read ()
  "Acknowledge the row's awaiting state, clearing the gold tint.
Stamps the buffer-local marked-read time to now.  The row will
re-enter `awaiting' state the next time the agent finishes
responding to a fresh prompt."
  (interactive)
  (let* ((row (agent-shell-dashboard--row-at-point))
         (sid (plist-get row :session-id))
         (buf (or (plist-get row :buffer)
                  (agent-shell-dashboard--find-live-buffer-for-session sid))))
    (unless (and buf (buffer-live-p buf))
      (user-error "Row has no live buffer to mark as read"))
    (with-current-buffer buf
      (setq agent-shell-dashboard--buffer-marked-read-time (current-time)))
    (agent-shell-dashboard-refresh)))

(defun agent-shell-dashboard--neighbor-row-id (excluded-id)
  "Return the session id of a row near point that isn't EXCLUDED-ID.
Prefers the next row in the same column, falls back to the
previous one.  Used to keep the cursor near where it was after a
row is removed and the dashboard re-renders."
  (cl-flet ((id-at-point ()
              (let ((id (plist-get (get-text-property (point) 'dg-row)
                                   :session-id)))
                (unless (equal id excluded-id) id))))
    (or (save-excursion (agent-shell-dashboard-next) (id-at-point))
        (save-excursion (agent-shell-dashboard-previous) (id-at-point)))))

(defun agent-shell-dashboard--goto-row-by-id (session-id)
  "Move point to the row whose `:session-id' is SESSION-ID, if present."
  (when session-id
    (cl-loop for pos in agent-shell-dashboard--row-positions
             for r = (get-text-property pos 'dg-row)
             when (equal session-id (plist-get r :session-id))
             return (progn (goto-char pos) t))))

(defun agent-shell-dashboard-stop ()
  "Stop the row's live session (kill its buffer + windows).
No-op on a closed row — use `K' to forget those permanently."
  (interactive)
  (let* ((row (agent-shell-dashboard--row-at-point))
         (buf (plist-get row :buffer))
         (session-id (plist-get row :session-id))
         (neighbor-id (agent-shell-dashboard--neighbor-row-id session-id)))
    (unless (and buf (buffer-live-p buf))
      (user-error "Row is not live; use K to forget closed sessions"))
    (when (yes-or-no-p (format "Stop %s? " (buffer-name buf)))
      (let ((windows (get-buffer-window-list buf nil nil)))
        (kill-buffer buf)
        (dolist (w windows)
          (when (window-live-p w)
            (ignore-errors (delete-window w)))))
      (agent-shell-dashboard-refresh)
      (agent-shell-dashboard--goto-row-by-id neighbor-id))))

(defun agent-shell-dashboard-forget ()
  "Forget the row's session permanently.
Kills the live buffer first when present, then strips the session
id from the active-sessions file and summary archive, and adds it
to the tombstone so JSONL enumeration won't resurface the row."
  (interactive)
  (let* ((row (agent-shell-dashboard--row-at-point))
         (buf (plist-get row :buffer))
         (session-id (plist-get row :session-id))
         (label (or (plist-get row :summary)
                    (and session-id
                         (substring session-id 0 (min 8 (length session-id))))
                    "this row"))
         (neighbor-id (agent-shell-dashboard--neighbor-row-id session-id)))
    (unless session-id
      (user-error "Row has no session id"))
    (when (yes-or-no-p
           (format (if (and buf (buffer-live-p buf))
                       "Forget \"%s\" permanently (also kills its live buffer)? "
                     "Forget \"%s\" permanently? ")
                   label))
      (when (and buf (buffer-live-p buf))
        (let ((windows (get-buffer-window-list buf nil nil)))
          (kill-buffer buf)
          (dolist (w windows)
            (when (window-live-p w)
              (ignore-errors (delete-window w))))))
      (agent-shell-dashboard--forget-session session-id)
      (agent-shell-dashboard-refresh)
      (agent-shell-dashboard--goto-row-by-id neighbor-id))))

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

(provide 'agent-shell-dashboard-commands)
;;; agent-shell-dashboard-commands.el ends here
