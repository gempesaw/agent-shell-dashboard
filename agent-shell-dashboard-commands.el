;;; agent-shell-dashboard-commands.el --- Mode, keymap, and interactive commands -*- lexical-binding: t; -*-

;;; Commentary:

;; The dashboard major mode, its keymap, and every interactive
;; command bound by it (navigate, visit, fork, kill, send, etc.),
;; plus the public `agent-shell-dashboard' entry point.

;;; Code:

(require 'dash)
(require 'agent-shell-dashboard-data)
(require 'agent-shell-dashboard-render)

(declare-function agent-shell--busy-submit "agent-shell-prompt-queue")
(declare-function agent-shell--insert-to-shell-buffer "agent-shell")

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
    ;; `m' marks for the layout.  `mark-as-read' keeps its command but
    ;; loses the key: it mattered when any unread row tinted gold, and
    ;; the awaiting tint is recency-gated now.
    (define-key map (kbd "m")   #'agent-shell-dashboard-toggle-mark)
    (define-key map (kbd "s")   #'agent-shell-dashboard-send)
    (define-key map (kbd "N")   #'agent-shell-dashboard-start-new-session)
    (define-key map (kbd "M")   #'agent-shell-dashboard-start-new-session-pick-repo)
    (define-key map (kbd "j")   #'agent-shell-dashboard-jump)
    (define-key map (kbd "r")   #'agent-shell-dashboard-reload)
    (define-key map (kbd "/")   #'agent-shell-dashboard-search)
    (define-key map (kbd "SPC") #'agent-shell-dashboard-toggle-mark)
    (define-key map (kbd "u")   #'agent-shell-dashboard-unmark)
    (define-key map (kbd "U")   #'agent-shell-dashboard-unmark-all)
    (define-key map (kbd "w")   #'agent-shell-dashboard-show-marked)
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

(defcustom agent-shell-dashboard-search-preview-limit 160
  "Characters of a row's last-prompt text made searchable.
Last prompts are unbounded — a pasted log or a long request can run to
thousands of characters — and every one of those characters is another
one each keystroke has to match against."
  :type 'integer
  :group 'agent-shell-dashboard)

(defun agent-shell-dashboard--read-literally (prompt collection)
  "Read from COLLECTION with predictable, non-fuzzy matching.
The ambient `completion-styles' typically ends in `flex', which matches
scattered characters: with candidates this long it matches nearly
everything, so a query with no real match returns noise ranked
alongside genuine hits.  Restrict matching to literal substrings —
space-separated terms still match in any order via orderless, each one
having to actually appear."
  (let ((completion-styles (if (featurep 'orderless)
                               '(orderless basic)
                             '(substring basic)))
        (completion-category-defaults nil)
        (completion-category-overrides nil))
    (let ((table (lambda (string predicate action)
                   (if (eq action 'metadata)
                       '(metadata (display-sort-function . identity)
                                  (cycle-sort-function . identity))
                     (complete-with-action action collection string predicate)))))
      (if (featurep 'orderless)
          (let ((orderless-matching-styles '(orderless-literal)))
            (completing-read prompt table nil t))
        (completing-read prompt table nil t)))))

(defun agent-shell-dashboard-search ()
  "Jump to a row by searching project / summary / preview text.
Searches the full row data, not just what fits in the displayed
column.  Tags captured by `agent-shell-dashboard-setup' show up
here even when truncated off-screen.

Matching is literal substring, not fuzzy, and each candidate is
collapsed onto one line and capped, so typing stays responsive and a
query either appears in a row or does not match it."
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
                  (summary (agent-shell-dashboard--collapse-whitespace
                            (plist-get row :summary)))
                  (preview (truncate-string-to-width
                            (agent-shell-dashboard--collapse-whitespace
                             (plist-get row :last-prompt-text))
                            agent-shell-dashboard-search-preview-limit))
                  (label (s-trim (format "%s | %s | %s"
                                         project summary preview))))
             (cons label it))
           agent-shell-dashboard--row-positions))
         (newest-first
          (-sort (lambda (a b)
                   (let ((ta (agent-shell-dashboard--row-time
                              (get-text-property (cdr a) 'dg-row)))
                         (tb (agent-shell-dashboard--row-time
                              (get-text-property (cdr b) 'dg-row))))
                     (cond ((and ta tb) (time-less-p tb ta))
                           (ta t))))
                 candidates))
         (choice (agent-shell-dashboard--read-literally
                  "Jump to row: " (-map #'car newest-first)))
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

(defun agent-shell-dashboard--fork-capable-p (buffer)
  "Non-nil when BUFFER's agent can fork its session."
  (and (buffer-live-p buffer)
       (map-elt (buffer-local-value 'agent-shell--state buffer)
                :supports-session-fork)))

(defun agent-shell-dashboard-fork ()
  "Fork the row's session into a shell with its own session id.
Asks the agent to fork, which copies the conversation into a new
session with a new id and its own transcript.  Resuming the same id in
a second shell -- what this used to do -- left both conversations
writing into one transcript tree, and a later resume walked a single
branch of it, so whichever half lost the race came back missing.

Falls back to resuming the id for closed rows, which have no live
session to fork, and for agents without fork support."
  (interactive)
  (let* ((row (agent-shell-dashboard--row-at-point))
         (buffer (plist-get row :buffer)))
    (if (agent-shell-dashboard--fork-capable-p buffer)
        (progn
          (with-current-buffer buffer (agent-shell-fork))
          ;; The fork is a round trip to the agent, so the new row does
          ;; not exist yet when this returns.
          (run-with-idle-timer 1 nil #'agent-shell-dashboard-refresh))
      (let ((new (agent-shell-dashboard--resume-session
                  (or (and (buffer-live-p buffer)
                           (agent-shell-dashboard--buffer-session-data buffer))
                      row))))
        (when new
          (message "Resumed %s; session shares the original's id"
                   (buffer-name new)))
        (agent-shell-dashboard-refresh)))))

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
      ;; Closing a session takes it out of the layout.  Pruned here on
      ;; the explicit close rather than from `kill-buffer-hook', which
      ;; would also fire for sessions whose buffers simply have not been
      ;; restored yet and quietly empty the mark set.  Must run before
      ;; the kill: once the buffer is gone the mark can only be matched
      ;; by session id, which would take this fork's siblings with it.
      (agent-shell-dashboard--unmark row)
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
      ;; Before the kill, so the mark is matched by buffer and this
      ;; fork's siblings keep theirs.
      (agent-shell-dashboard--unmark row)
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

;;; Marking and the marked-session layout

(defcustom agent-shell-dashboard-layout-max-columns 4
  "Most columns the marked-session layout will use.
Beyond this many marked sessions the layout starts stacking rows
within each column instead of adding more columns."
  :type 'integer
  :group 'agent-shell-dashboard)

(defun agent-shell-dashboard-toggle-mark ()
  "Toggle the mark on the row at point and advance."
  (interactive)
  (let* ((row (agent-shell-dashboard--row-at-point))
         (sid (or (plist-get row :session-id)
                  (user-error "Row has no session id")))
         (marked (agent-shell-dashboard--toggle-mark row)))
    (agent-shell-dashboard-refresh)
    (agent-shell-dashboard--goto-row-by-id sid)
    (agent-shell-dashboard-next)
    (message "%s (%d marked)"
             (if marked "Marked" "Unmarked")
             (length agent-shell-dashboard--marked-ids))))

(defun agent-shell-dashboard-unmark ()
  "Remove the mark from the row at point."
  (interactive)
  (let* ((row (agent-shell-dashboard--row-at-point))
         (sid (plist-get row :session-id)))
    (agent-shell-dashboard--unmark row)
    (agent-shell-dashboard-refresh)
    (agent-shell-dashboard--goto-row-by-id sid)))

(defun agent-shell-dashboard-unmark-all ()
  "Clear every mark."
  (interactive)
  (agent-shell-dashboard--set-marks nil)
  (agent-shell-dashboard-refresh)
  (message "Cleared all marks"))

(defun agent-shell-dashboard--marked-buffers ()
  "Live buffers for the marked sessions, sorted by buffer name.
Sorted rather than kept in mark order so a given set of sessions always
lands in the same windows: unmarking and re-marking one should not
shuffle the others out from under muscle memory.

Marked sessions whose buffer is gone are skipped rather than resumed;
resuming would start processes as a side effect of arranging windows."
  (let (buffers missing)
    (dolist (entry (agent-shell-dashboard--ensure-marks))
      (let* ((name (plist-get entry :buffer))
             (sid (plist-get entry :session))
             (buf (or (and name (get-buffer name))
                      (and sid (agent-shell-dashboard--find-live-buffer-for-session
                                sid)))))
        (if (buffer-live-p buf)
            ;; Two entries can resolve to the same buffer — a fork whose
            ;; sibling died falls back to session identity and lands on
            ;; whatever buffer that id still resolves to.  One window each.
            (cl-pushnew buf buffers)
          (push entry missing))))
    (when missing
      (message "%d marked session(s) have no live buffer; skipped"
               (length missing)))
    (sort buffers (lambda (a b) (string< (buffer-name a) (buffer-name b))))))

;; Telling four session windows apart by their scrollback means reading
;; a lot of text.  Each session already has a captured summary, so the
;; layout puts it in that window's header line, larger and bold — the
;; same glance-and-recognize trick as the dashboard's jump labels, but
;; persistent rather than transient.

(defcustom agent-shell-dashboard-layout-label t
  "Whether the marked-session layout labels each window with its summary."
  :type 'boolean
  :group 'agent-shell-dashboard)

(defcustom agent-shell-dashboard-layout-label-height 1.4
  "Text scale of the layout's per-window summary label."
  :type 'number
  :group 'agent-shell-dashboard)

(defvar-local agent-shell-dashboard--saved-header-line 'unset
  "This buffer's `header-line-format' before the layout replaced it.")

(defun agent-shell-dashboard--layout-label-text (buf)
  "One-line label for BUF: its summary, else its buffer name."
  (with-current-buffer buf
    (let ((summary (agent-shell-dashboard--collapse-whitespace
                    (agent-shell-dashboard--clean-summary
                     agent-shell-dashboard--buffer-summary))))
      (if (s-blank? summary) (buffer-name) summary))))

(defun agent-shell-dashboard--apply-layout-label (buf)
  "Put BUF's summary in its header line, remembering what was there."
  (when agent-shell-dashboard-layout-label
    (with-current-buffer buf
      (when (eq agent-shell-dashboard--saved-header-line 'unset)
        (setq agent-shell-dashboard--saved-header-line header-line-format))
      (setq header-line-format
            (propertize
             (concat " " (agent-shell-dashboard--layout-label-text buf))
             'face `(:height ,agent-shell-dashboard-layout-label-height
                     :weight bold
                     :foreground "#ffb8d1"))))))

(defun agent-shell-dashboard--clear-layout-label (buf)
  "Restore BUF's header line if the layout replaced it."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (unless (eq agent-shell-dashboard--saved-header-line 'unset)
        (setq header-line-format agent-shell-dashboard--saved-header-line)
        (setq agent-shell-dashboard--saved-header-line 'unset)))))

(defun agent-shell-dashboard-clear-layout-labels ()
  "Drop the layout's summary labels from every session buffer."
  (interactive)
  (dolist (buf (agent-shell-dashboard--all-buffers))
    (agent-shell-dashboard--clear-layout-label buf))
  (message "Cleared layout labels"))

(defun agent-shell-dashboard--layout-distribution (n ncols)
  "Split N windows across NCOLS columns, leftmost columns taking extras.
Five across four columns gives (2 1 1 1): the first column stacks two
rows and the rest stay single."
  (let ((base (/ n ncols))
        (extra (% n ncols)))
    (cl-loop for i from 0 below ncols
             collect (+ base (if (< i extra) 1 0)))))

(defun agent-shell-dashboard--split-n (window n side)
  "Split WINDOW into N windows along SIDE, returned in visual order.
Sizes are left uneven here and evened out by `balance-windows'."
  (let ((wins (list window)))
    (dotimes (_ (1- n))
      (setq wins (nconc wins (list (split-window (car (last wins)) nil side)))))
    wins))

(defun agent-shell-dashboard-show-marked ()
  "Take over the frame with one window per marked session.
Up to `agent-shell-dashboard-layout-max-columns' marked sessions each
get their own column; beyond that, columns start stacking rows."
  (interactive)
  (let* ((buffers (agent-shell-dashboard--marked-buffers))
         (n (length buffers)))
    (unless buffers
      (user-error "No marked sessions with live buffers"))
    (let* ((ncols (min agent-shell-dashboard-layout-max-columns n))
           (dist (agent-shell-dashboard--layout-distribution n ncols))
           (rest buffers))
      ;; Labels are cleared wholesale first so a session dropped from the
      ;; marks does not keep wearing one.
      (dolist (buf (agent-shell-dashboard--all-buffers))
        (agent-shell-dashboard--clear-layout-label buf))
      (delete-other-windows)
      (let ((columns (agent-shell-dashboard--split-n
                      (selected-window) ncols 'right)))
        (cl-loop for col in columns
                 for count in dist
                 do (dolist (row (agent-shell-dashboard--split-n col count 'below))
                      (when rest
                        (let ((buf (pop rest)))
                          (agent-shell-dashboard--apply-layout-label buf)
                          (set-window-buffer row buf)))))
        (balance-windows)
        (select-window (car columns))))
    (message "Showing %d marked session%s" n (if (= n 1) "" "s"))))

;;; Unforking

(defcustom agent-shell-dashboard-unfork-timeout 900
  "Seconds to wait for a fork to write its summary before giving up."
  :type 'integer
  :group 'agent-shell-dashboard)

(defun agent-shell-dashboard--unfork-source ()
  (cond
   ((derived-mode-p 'agent-shell-mode) (current-buffer))
   ((and (derived-mode-p 'agent-shell-dashboard-mode)
         (buffer-live-p (plist-get (agent-shell-dashboard--row-at-point) :buffer)))
    (plist-get (agent-shell-dashboard--row-at-point) :buffer))
   ((fboundp 'agent-shell--current-shell) (agent-shell--current-shell))))

(defun agent-shell-dashboard--session-id (buffer)
  (map-nested-elt (buffer-local-value 'agent-shell--state buffer) '(:session :id)))

(defun agent-shell-dashboard--transcript (buffer)
  (let ((file (buffer-local-value 'agent-shell--transcript-file buffer)))
    (and file (file-exists-p file) file)))

(defun agent-shell-dashboard--fork-relatives (buffer)
  (agent-shell-dashboard--record-fork-lineage)
  (let ((root (agent-shell-dashboard--fork-root
               (agent-shell-dashboard--session-id buffer))))
    (sort (--filter (and (not (eq it buffer))
                         (agent-shell-dashboard--session-id it)
                         (equal root (agent-shell-dashboard--fork-root
                                      (agent-shell-dashboard--session-id it))))
                    (agent-shell-dashboard--all-buffers))
          (lambda (a b)
            (let ((da (agent-shell-dashboard--fork-depth a))
                  (db (agent-shell-dashboard--fork-depth b)))
              (if (/= da db)
                  (< da db)
                (string< (or (agent-shell-dashboard--transcript a) "~")
                         (or (agent-shell-dashboard--transcript b) "~"))))))))

(defun agent-shell-dashboard--fork-depth (buffer)
  (let ((lineage (agent-shell-dashboard--fork-lineage))
        (id (agent-shell-dashboard--session-id buffer))
        (depth 0))
    (while (and (assoc id lineage) (< depth 100))
      (setq id (cdr (assoc id lineage))
            depth (1+ depth)))
    depth))

(defun agent-shell-dashboard--pick-unfork-target (relatives)
  (if (null (cdr relatives))
      (car relatives)
    (let* ((choices (--map-indexed
                     (cons (format "%s%s  %s"
                                   (buffer-name it)
                                   (if (zerop it-index) " (oldest)" "")
                                   (or (agent-shell-dashboard--collapse-whitespace
                                        (buffer-local-value
                                         'agent-shell-dashboard--buffer-summary it))
                                       ""))
                           it)
                     relatives))
           (table (lambda (string pred action)
                    (if (eq action 'metadata)
                        '(metadata (display-sort-function . identity))
                      (complete-with-action action (mapcar #'car choices) string pred)))))
      (cdr (assoc (completing-read "Unfork into: " table nil t nil nil (caar choices))
                  choices)))))

(defun agent-shell-dashboard--first-prompt (transcript)
  (when transcript
    (with-temp-buffer
      (insert-file-contents transcript nil 0 16384)
      (goto-char (point-min))
      (when (re-search-forward "^## User (" nil t)
        (forward-line 1)
        (skip-chars-forward " \t\n")
        (truncate-string-to-width
         (buffer-substring-no-properties (point) (line-end-position)) 120)))))

(defun agent-shell-dashboard--unfork-file (source)
  (let ((transcript (agent-shell-dashboard--transcript source)))
    (expand-file-name
     (format "%s.md" (agent-shell-dashboard--session-id source))
     (if transcript
         (expand-file-name "../unforks/" (file-name-directory transcript))
       (expand-file-name "agent-shell-unforks/" temporary-file-directory)))))

(defun agent-shell-dashboard--submit (buffer prompt)
  (with-current-buffer buffer
    (if (shell-maker-busy)
        (agent-shell--busy-submit :prompt prompt)
      (agent-shell--insert-to-shell-buffer :text prompt :submit t :no-focus t))))

(defun agent-shell-dashboard--unfork-deliver (source target file close)
  (let ((transcript (agent-shell-dashboard--transcript source)))
    (agent-shell-dashboard--submit
     target
     (concat
      (format "Merging back a fork. %s was forked from this conversation and went on to do work you have not seen."
              (buffer-name source))
      (format "\n\nIts account of everything since the fork is at:\n  %s" file)
      (when transcript
        (format "\n\nThe full record of that work, if the account is not enough:\n  %s" transcript))
      "\n\nRead the account, then tell me in a few lines what changed and what you are carrying forward."))
    (when close
      (agent-shell-dashboard--unmark (list :buffer source
                                           :session-id (agent-shell-dashboard--session-id source)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer source)))
    (message "Unforked %s into %s" (if close "and closed the fork" (buffer-name source))
             (buffer-name target))))

(defun agent-shell-dashboard--unfork-wait (source target file close deadline)
  (cond
   ((not (and (buffer-live-p source) (buffer-live-p target)))
    (message "Unfork abandoned: a session went away"))
   ((and (file-exists-p file)
         (> (file-attribute-size (file-attributes file)) 0)
         (not (with-current-buffer source (shell-maker-busy))))
    (agent-shell-dashboard--unfork-deliver source target file close))
   ((time-less-p deadline nil)
    (message "Unfork gave up waiting for %s" file))
   (t
    (run-at-time 3 nil #'agent-shell-dashboard--unfork-wait
                 source target file close deadline))))

(defun agent-shell-dashboard-unfork (&optional close)
  (interactive "P")
  (let* ((source (or (agent-shell-dashboard--unfork-source)
                     (user-error "Not in an agent-shell session")))
         (relatives (or (agent-shell-dashboard--fork-relatives source)
                        (user-error "%s has no live fork relatives" (buffer-name source))))
         (target (agent-shell-dashboard--pick-unfork-target relatives)))
    (agent-shell-dashboard--unfork-into source target close)))

(defun agent-shell-dashboard--unfork-into (source target close)
  (let* ((file (agent-shell-dashboard--unfork-file source))
         (since (agent-shell-dashboard--first-prompt
                 (agent-shell-dashboard--transcript source))))
    (make-directory (file-name-directory file) t)
    (when (file-exists-p file) (delete-file file))
    (agent-shell-dashboard--submit
     source
     (concat
      (format "We are folding this session back into %s, which shares everything that happened before this session was forked off. "
              (buffer-name target))
      (format "Write to %s an account of only what happened after the fork" file)
      (if since (format ", starting from my message that began %S" since) "")
      ". Cover what was decided, what changed (files, commits, branches, PRs), what was learned, and what is still open. Do not restate anything from before the fork; the other session already has it. Plain markdown, as short as it can be while still letting the other session pick up this work. Reply only with \"written\" when the file is saved."))
    (agent-shell-dashboard--unfork-wait
     source target file close
     (time-add nil agent-shell-dashboard-unfork-timeout))
    (message "Asked %s to write up its fork; will hand it to %s when done"
             (buffer-name source) (buffer-name target))))

(defun agent-shell-dashboard ()
  "Open the agent-shell dashboard, taking over the current frame.
The pre-existing window layout is restored when you press `q'.
Keys: RET visit, o open-in-other-window, n/p navigate, f fork,
k kill, s send prompt, N new session, M new session (pick repo),
SPC/m mark, u unmark, U unmark all, w show marked layout,
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
