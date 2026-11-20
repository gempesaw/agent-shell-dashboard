;;; agent-shell-dashboard-web.el --- The dashboard in a browser -*- lexical-binding: t; -*-

;; Author: Daniel Gempesaw <gempesaw@gmail.com>

;;; Commentary:

;; Serves the dashboard over HTTP from inside Emacs, so another machine
;; on the network can browse sessions, read a session's buffer and send
;; it prompts.  `agent-shell-dashboard-web-start' starts the server and
;; puts its URL on the kill ring.  There is no authentication: anyone who
;; can reach the port can send prompts that run with the session's
;; permissions.

;;; Code:

(require 'json)
(require 'map)
(require 'color)
(require 'url-util)
(require 'agent-shell-dashboard-data)
(require 'agent-shell-dashboard-render)

(declare-function agent-shell--busy-submit "agent-shell-prompt-queue")
(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function shell-maker-busy "shell-maker")

(defcustom agent-shell-dashboard-web-port 6969
  "Port the web dashboard listens on."
  :type 'integer
  :group 'agent-shell-dashboard)

(defcustom agent-shell-dashboard-web-host "::"
  "Address the web dashboard binds to.
The default listens on every interface, IPv4 and IPv6, so other machines
can reach it however they resolve this host's name; \"::1\" keeps it
local."
  :type 'string
  :group 'agent-shell-dashboard)

(defcustom agent-shell-dashboard-web-buffer-limit 400000
  "Characters from the end of a session buffer sent to the browser."
  :type 'integer
  :group 'agent-shell-dashboard)

(defcustom agent-shell-dashboard-web-font-directories
  '("~/Library/Fonts" "/Library/Fonts" "~/.local/share/fonts")
  "Directories searched for files of the default face's font family.
Matching files are served to the page, so it renders in the same font as
Emacs on machines that do not have it installed."
  :type '(repeat directory)
  :group 'agent-shell-dashboard)

(defvar agent-shell-dashboard-web--server nil)
(defvar agent-shell-dashboard-web--snapshot nil)
(defvar agent-shell-dashboard-web--closed nil
  "Closed rows and their serialized form: (TIME LIVE-IDS ROWS . SERIALIZED).")
(defvar agent-shell-dashboard-web--transcripts (make-hash-table :test 'eq :weakness 'key))
(defvar agent-shell-dashboard-web--last-request nil)
(defvar agent-shell-dashboard-web--maintenance nil)

(defconst agent-shell-dashboard-web--page
  (expand-file-name "agent-shell-dashboard-web.html"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun agent-shell-dashboard-web-url ()
  "Put the web dashboard's URL on the kill ring."
  (interactive)
  (let ((url (format "http://%s:%d/"
                     (concat (string-trim
                              (shell-command-to-string "scutil --get LocalHostName"))
                             ".local")
                     agent-shell-dashboard-web-port)))
    (kill-new url)
    (message "%s (copied)" url)
    url))

;;;; Serialization

(defun agent-shell-dashboard-web--page-version ()
  (format "%d" (float-time (file-attribute-modification-time
                            (file-attributes agent-shell-dashboard-web--page)))))

(defun agent-shell-dashboard-web--hex (color)
  (if-let* ((rgb (and (stringp color) (color-name-to-rgb color))))
      (apply #'color-rgb-to-hex (append rgb '(2)))
    color))

(defun agent-shell-dashboard-web--color (face attribute)
  (let ((value (pcase attribute
                 (:foreground (face-foreground face nil t))
                 (:background (face-background face nil t)))))
    (if (and (stringp value) (not (string-prefix-p "unspecified" value)))
        (agent-shell-dashboard-web--hex value)
      :null)))

(defun agent-shell-dashboard-web--glyph (row)
  (let* ((glyph (agent-shell-dashboard--status-glyph
                 (plist-get row :status)
                 (agent-shell-dashboard--row-is-today row)))
         (face (get-text-property 0 'face glyph)))
    (list :char (substring-no-properties glyph)
          :color (cond ((and (consp face) (plist-get face :foreground))
                        (agent-shell-dashboard-web--hex (plist-get face :foreground)))
                       ((symbolp face) (agent-shell-dashboard-web--color face :foreground))
                       (t :null)))))

(defun agent-shell-dashboard-web--row (row fork-index)
  (let* ((buffer (plist-get row :buffer))
         (live (buffer-live-p buffer))
         (face (agent-shell-dashboard--row-face row))
         (fork (and fork-index (ht-get fork-index (plist-get row :session-id))))
         (time (agent-shell-dashboard--row-time row)))
    (list :buffer (if live (buffer-name buffer) :null)
          :session_id (or (plist-get row :session-id) :null)
          :live (if live t :false)
          :status (symbol-name (or (plist-get row :status) 'unknown))
          :ago (cond (time :null)
                     ((eq (plist-get row :status) 'closed) "closed")
                     (t "—"))
          :time (if time (float-time time) 0)
          :summary (agent-shell-dashboard--collapse-whitespace
                    (agent-shell-dashboard--clean-summary (plist-get row :summary)))
          :preview (truncate-string-to-width
                    (agent-shell-dashboard--collapse-whitespace
                     (plist-get row :last-prompt-text))
                    300)
          :tint (if face (agent-shell-dashboard-web--color face :background) :null)
          :glyph (agent-shell-dashboard-web--glyph row)
          :fork (if fork
                    (list :letter (string (+ ?a (mod fork 26)))
                          :color (nth (mod fork (length agent-shell-dashboard-fork-colors))
                                      agent-shell-dashboard-fork-colors))
                  :null)
          :cwd (or (plist-get row :cwd) :null)
          :marked (if (agent-shell-dashboard-row-marked-p row) t :false)
          :recency (symbol-name (or (and live (agent-shell-dashboard--row-recency row)) 'none)))))

(defun agent-shell-dashboard-web--live-ids ()
  (sort (delq nil (mapcar (lambda (buffer)
                            (map-nested-elt (buffer-local-value 'agent-shell--state buffer)
                                            '(:session :id)))
                          (agent-shell-dashboard--all-buffers)))
        #'string<))

(defun agent-shell-dashboard-web--refresh-closed ()
  (let ((rows (--remove (plist-get it :buffer) (agent-shell-dashboard--rows)))
        (serialized (make-hash-table :test 'equal)))
    (dolist (row rows)
      (puthash (plist-get row :session-id) (agent-shell-dashboard-web--row row nil) serialized))
    (setq agent-shell-dashboard-web--closed
          (cons (current-time)
                (cons (agent-shell-dashboard-web--live-ids)
                      (cons rows serialized))))))

(defun agent-shell-dashboard-web--closed-stale-p ()
  (or (null agent-shell-dashboard-web--closed)
      (> (float-time (time-subtract nil (car agent-shell-dashboard-web--closed))) 60)
      (not (equal (cadr agent-shell-dashboard-web--closed)
                  (agent-shell-dashboard-web--live-ids)))))

(defun agent-shell-dashboard-web--maintain ()
  "Rebuild closed rows while Emacs is idle and a browser is watching.
They come from disk and rarely change, so rebuilding them on request
would stall Emacs for every poll; live rows are cheap and stay fresh."
  (when (and agent-shell-dashboard-web--last-request
             (< (float-time (time-subtract nil agent-shell-dashboard-web--last-request)) 120)
             (agent-shell-dashboard-web--closed-stale-p))
    (agent-shell-dashboard-web--refresh-closed)))

(defun agent-shell-dashboard-web--sessions ()
  (unless agent-shell-dashboard-web--closed
    (agent-shell-dashboard-web--refresh-closed))
  (let* ((live (delq nil (mapcar #'agent-shell-dashboard--row-from-buffer
                                 (agent-shell-dashboard--all-buffers))))
         (live-ids (mapcar (lambda (row) (plist-get row :session-id)) live))
         (closed (--remove (member (plist-get it :session-id) live-ids)
                           (car (cddr agent-shell-dashboard-web--closed))))
         (serialized (cdr (cddr agent-shell-dashboard-web--closed)))
         (rows (sort (append live closed) #'agent-shell-dashboard--row-less-p))
         (fork-index (agent-shell-dashboard--compute-fork-index rows))
         (groups (agent-shell-dashboard--group-rows rows))
         (pinned (--keep (assoc it groups) agent-shell-dashboard-pinned-projects)))
    (list :theme (list :bg (agent-shell-dashboard-web--color 'default :background)
                       :fg (agent-shell-dashboard-web--color 'default :foreground)
                       :shadow (agent-shell-dashboard-web--color 'shadow :foreground)
                       :header (agent-shell-dashboard-web--color 'font-lock-function-name-face
                                                                 :foreground))
          :page (agent-shell-dashboard-web--page-version)
          :live (length live)
          :closed (length closed)
          :pinned (vconcat (mapcar #'car pinned))
          :groups (vconcat
                   (mapcar (lambda (group)
                             (list :project (if (s-blank? (car group)) "(none)" (car group))
                                   :rows (vconcat
                                          (mapcar (lambda (row)
                                                    (if (plist-get row :buffer)
                                                        (agent-shell-dashboard-web--row row fork-index)
                                                      (or (gethash (plist-get row :session-id) serialized)
                                                          (agent-shell-dashboard-web--row row nil))))
                                                  (cdr group)))))
                           (append pinned (--remove (memq it pinned) groups)))))))

(defun agent-shell-dashboard-web--sessions-json (etag)
  (setq agent-shell-dashboard-web--last-request (current-time))
  (let ((cached agent-shell-dashboard-web--snapshot))
    (unless (and cached (< (float-time (time-subtract nil (car cached))) 2))
      (let* ((json (json-serialize (agent-shell-dashboard-web--sessions)))
             (hash (secure-hash 'md5 json)))
        (setq cached (list (current-time) hash
                           (concat "{\"etag\":\"" hash "\"," (substring json 1)))
              agent-shell-dashboard-web--snapshot cached)))
    (if (equal etag (nth 1 cached))
        (list :unchanged t :etag etag)
      (nth 2 cached))))

(defvar agent-shell-dashboard-web--waiters nil)
(defvar agent-shell-dashboard-web--waiter-timer nil)

(defconst agent-shell-dashboard-web-long-poll-seconds 25)

(defun agent-shell-dashboard-web--session-state (buffer)
  (with-current-buffer buffer
    (list (buffer-modified-tick)
          (and (shell-maker-busy) t)
          (length (map-elt agent-shell--state :pending-prompts)))))

(defun agent-shell-dashboard-web--wait-for-change (process buffer args)
  (push (list process buffer
              (list (plist-get args :tick) (eq (plist-get args :busy) t) (plist-get args :queued))
              (time-add nil agent-shell-dashboard-web-long-poll-seconds))
        agent-shell-dashboard-web--waiters)
  (unless agent-shell-dashboard-web--waiter-timer
    (setq agent-shell-dashboard-web--waiter-timer
          (run-at-time 0.3 0.3 #'agent-shell-dashboard-web--check-waiters))))

(defun agent-shell-dashboard-web--check-waiters ()
  (setq agent-shell-dashboard-web--waiters
        (seq-filter
         (pcase-lambda (`(,process ,buffer ,known ,deadline))
           (cond
            ((not (process-live-p process)) nil)
            ((not (buffer-live-p buffer))
             (agent-shell-dashboard-web--json process 200 '(:gone t))
             nil)
            ((or (time-less-p deadline nil)
                 (not (equal known (agent-shell-dashboard-web--session-state buffer))))
             (condition-case err
                 (agent-shell-dashboard-web--json
                  process 200 (agent-shell-dashboard-web--session buffer (car known)))
               (error (agent-shell-dashboard-web--json
                       process 500 (list :error (error-message-string err)))))
             nil)
            (t t)))
         agent-shell-dashboard-web--waiters))
  (when (and (null agent-shell-dashboard-web--waiters)
             agent-shell-dashboard-web--waiter-timer)
    (cancel-timer agent-shell-dashboard-web--waiter-timer)
    (setq agent-shell-dashboard-web--waiter-timer nil)))

(defun agent-shell-dashboard-web--session-buffer (name)
  (let ((buffer (and (stringp name) (get-buffer name))))
    (unless (and buffer (with-current-buffer buffer (derived-mode-p 'agent-shell-mode)))
      (signal 'user-error (list (format "No agent-shell buffer %S" name))))
    buffer))

(defun agent-shell-dashboard-web--face-style (face)
  (let ((faces (if (and (consp face) (not (keywordp (car face)))) face (list face)))
        (default-fg (face-foreground 'default nil t))
        (default-bg (face-background 'default nil t))
        fg bg bold italic underline)
    (dolist (f faces)
      (let ((get (lambda (attribute)
                   (let ((value (cond ((and (symbolp f) (facep f))
                                       (face-attribute f attribute nil t))
                                      ((consp f) (plist-get f attribute)))))
                     (unless (or (null value) (eq value 'unspecified)) value)))))
        (unless fg
          (let ((v (funcall get :foreground)))
            (when (and (stringp v) (not (equal v default-fg))) (setq fg v))))
        (unless bg
          (let ((v (funcall get :background)))
            (when (and (stringp v) (not (equal v default-bg))) (setq bg v))))
        (unless bold
          (setq bold (memq (funcall get :weight) '(bold semi-bold extra-bold ultra-bold heavy))))
        (unless italic
          (setq italic (memq (funcall get :slant) '(italic oblique))))
        (unless underline
          (setq underline (funcall get :underline)))))
    (list :fg (if fg (agent-shell-dashboard-web--hex fg) :null)
          :bg (if bg (agent-shell-dashboard-web--hex bg) :null)
          :bold (if bold t :false) :italic (if italic t :false)
          :underline (if underline t :false))))

(defcustom agent-shell-dashboard-web-shell-languages
  '("bash" "sh" "shell" "zsh" "console" "shellsession")
  "Code block languages the page offers to run in a console."
  :type '(repeat string)
  :group 'agent-shell-dashboard)

(defvar agent-shell-dashboard-web-console-run-function nil
  "Function of (SESSION-BUFFER COMMAND) running COMMAND in a console.
Nil leaves code blocks without a run button.")

(defvar agent-shell-dashboard-web-console-split-function nil
  "Function splitting a snippet's text into a list of commands.")

(defvar agent-shell-dashboard-web-console-open-function nil
  "Function of (SESSION-BUFFER) starting its console ahead of the first run.")

(defvar agent-shell-dashboard-web-console-text-function nil
  "Function of (SESSION-BUFFER) returning the console's recent output.")

(defun agent-shell-dashboard-web--block-language (body-start)
  (let ((start (or (previous-single-property-change
                    body-start 'agent-shell-markdown-source-block-rendered)
                   (point-min))))
    (car (split-string (buffer-substring-no-properties start body-start)))))

(defun agent-shell-dashboard-web--runs (start end &optional reveal)
  "Runs between START and END, as (RUNS STYLES SECTIONS).
Each run is [TEXT STYLE HIDDEN SECTION ROLE URL], SECTION an index into
SECTIONS or -1 and URL the link target, if any.  Unless REVEAL, invisible text is left out: it is most
of a session buffer, and copying it on every update costs a garbage
collection.  A collapsed section's body becomes one empty marker run, so
the page knows it exists and can fetch it when opened."
  (let ((styles (list (list :fg :null :bg :null :bold :false :italic :false :underline :false)))
        (style-index (make-hash-table :test 'equal))
        (sections nil)
        (section-index (make-hash-table :test 'equal))
        runs
        (snippet-count 0)
        (snippet-end start)
        (snippet -1)
        (pos start))
    (while (< pos end)
      (let* ((next (min end (next-char-property-change pos end)))
             (role (get-text-property pos 'agent-shell-ui-section))
             (id (and role (map-elt (get-text-property pos 'agent-shell-ui-state) :qualified-id)))
             (code (pcase role
                     ('indicator "i")
                     ((or 'label-left 'label-right) "l")
                     ('body "b")
                     (_ "")))
             (hidden (and (not reveal) (invisible-p pos)))
             (snippet
              (cond
               ((not (and agent-shell-dashboard-web-console-run-function
                          (get-text-property pos 'agent-shell-markdown-source-block-body)))
                -1)
               ((< pos snippet-end) snippet)
               (t
                (let ((body-start (if (and (> pos (point-min))
                                           (get-text-property
                                            (1- pos) 'agent-shell-markdown-source-block-body))
                                      (or (previous-single-property-change
                                           pos 'agent-shell-markdown-source-block-body)
                                          (point-min))
                                    pos)))
                  (setq snippet-end (or (next-single-property-change
                                         pos 'agent-shell-markdown-source-block-body nil end)
                                        end)
                        snippet (if (member (agent-shell-dashboard-web--block-language body-start)
                                            agent-shell-dashboard-web-shell-languages)
                                    (prog1 snippet-count (cl-incf snippet-count))
                                  -1))))))
             (section (if id
                          (or (gethash id section-index)
                              (progn (push id sections)
                                     (puthash id (1- (length sections)) section-index)))
                        -1)))
        (cond
         ((and hidden (not (equal code "b"))))
         ((and hidden
               (let ((last (car runs)))
                 (and last (= (aref last 2) 1) (= (aref last 3) section)))))
         (t
          (let* ((face (or (get-char-property pos 'face)
                           (get-char-property pos 'font-lock-face)))
                 (style (if (or (null face) (eq face 'default))
                            0
                          (or (gethash face style-index)
                              (progn
                                (setq styles (append styles (list (agent-shell-dashboard-web--face-style face))))
                                (puthash face (1- (length styles)) style-index))))))
            (push (vector (if hidden "" (buffer-substring-no-properties pos next))
                          style (if hidden 1 0) section code
                          (let ((url (or (get-text-property pos 'agent-shell-markdown-url)
                                         (get-text-property pos 'browse-url-data))))
                            (if (and (not hidden) (stringp url)) url ""))
                          snippet)
                  runs))))
        (setq pos next)))
    (list (vconcat (nreverse runs)) (vconcat styles) (vconcat (nreverse sections)))))

(defun agent-shell-dashboard-web--session (buffer known-tick)
  (with-current-buffer buffer
    (let ((tick (buffer-modified-tick))
          (busy (if (shell-maker-busy) t :false))
          (queued (vconcat (map-elt agent-shell--state :pending-prompts))))
      (if (equal tick known-tick)
          (list :tick tick :unchanged t :busy busy :queued queued)
        (let ((cached (gethash buffer agent-shell-dashboard-web--transcripts)))
          (if (equal (car cached) tick)
              (cdr cached)
            (let* ((start (max (point-min) (- (point-max) agent-shell-dashboard-web-buffer-limit)))
                   (runs (agent-shell-dashboard-web--runs start (point-max)))
                   (json (json-serialize
                          (list :tick tick
                                :busy busy
                                :queued queued
                                :truncated (if (> start (point-min)) t :false)
                                :summary (or agent-shell-dashboard--buffer-summary :null)
                                :runs (nth 0 runs)
                                :styles (nth 1 runs)
                                :sections (nth 2 runs)))))
              (puthash buffer (cons tick json) agent-shell-dashboard-web--transcripts)
              json)))))))

(defun agent-shell-dashboard-web--section (buffer section)
  (with-current-buffer buffer
    (let ((pos (max (point-min) (- (point-max) agent-shell-dashboard-web-buffer-limit)))
          start end)
      (while (and (< pos (point-max)) (not end))
        (let ((next (min (next-single-property-change pos 'agent-shell-ui-section nil (point-max))
                         (next-single-property-change pos 'agent-shell-ui-state nil (point-max)))))
          (when (and (eq (get-text-property pos 'agent-shell-ui-section) 'body)
                     (equal (map-elt (get-text-property pos 'agent-shell-ui-state) :qualified-id)
                            section))
            (setq start pos end next))
          (setq pos next)))
      (unless start
        (signal 'user-error (list (format "No section %s" section))))
      (let ((runs (agent-shell-dashboard-web--runs start end t)))
        (list :runs (nth 0 runs) :styles (nth 1 runs))))))

(defun agent-shell-dashboard-web--projects ()
  "Directories a session may be started in, most recently used first.
Only places the dashboard already knows — projects with sessions and
projectile's list — so the page cannot start an agent anywhere else."
  (let ((seen (make-hash-table :test 'equal))
        projects)
    (dolist (dir (append
                  (delq nil (mapcar (lambda (row) (plist-get row :cwd))
                                    (agent-shell-dashboard--rows)))
                  (and (boundp 'projectile-known-projects) projectile-known-projects)))
      (let ((dir (file-name-as-directory (expand-file-name dir))))
        (when (and (not (gethash dir seen)) (file-directory-p dir))
          (puthash dir t seen)
          (push (list :name (file-name-nondirectory (directory-file-name dir)) :dir dir)
                projects))))
    (nreverse projects)))

(defun agent-shell-dashboard-web--new-session (dir)
  (let ((dir (file-name-as-directory (expand-file-name (or dir "")))))
    (unless (--any? (equal (plist-get it :dir) dir) (agent-shell-dashboard-web--projects))
      (signal 'user-error (list (format "Not a known project: %s" dir))))
    (let* ((config-fn (cdar agent-shell-dashboard-identifier-to-config-fn-alist))
           (before (agent-shell-dashboard--all-buffers))
           (default-directory dir)
           (agent-shell-cwd-function (lambda () dir))
           (agent-shell-session-strategy 'new))
      (save-window-excursion
        (agent-shell-start :config (funcall config-fn)))
      (setq agent-shell-dashboard-web--snapshot nil)
      (let ((buffer (car (-difference (agent-shell-dashboard--all-buffers) before))))
        (list :buffer (if (buffer-live-p buffer) (buffer-name buffer) :null))))))

(defun agent-shell-dashboard-web--send (buffer prompt)
  (setq prompt (s-trim (or prompt "")))
  (when (s-blank? prompt)
    (signal 'user-error (list "Empty prompt")))
  (with-current-buffer buffer
    (if (shell-maker-busy)
        (agent-shell--busy-submit :prompt prompt)
      (agent-shell--insert-to-shell-buffer :text prompt :submit t :no-focus t)))
  (list :ok t))

(defun agent-shell-dashboard-web--resume (session-id)
  (let ((row (--find (equal (plist-get it :session-id) session-id)
                     (agent-shell-dashboard--rows))))
    (unless row
      (signal 'user-error (list (format "No session %s" session-id))))
    (let ((buffer (or (agent-shell-dashboard--find-live-buffer-for-session session-id)
                      (save-window-excursion
                        (agent-shell-dashboard--resume-session row)))))
      (setq agent-shell-dashboard-web--snapshot nil)
      (list :buffer (if (buffer-live-p buffer) (buffer-name buffer) :null)))))

;;;; Fonts

(defun agent-shell-dashboard-web--font-family ()
  (let ((family (face-attribute 'default :family nil t)))
    (and (stringp family) family)))

(defun agent-shell-dashboard-web--font-files ()
  (when-let* ((family (agent-shell-dashboard-web--font-family))
              (prefix (concat (replace-regexp-in-string " " "" family) "-")))
    (seq-mapcat
     (lambda (directory)
       (and (file-directory-p directory)
            (directory-files (expand-file-name directory) t
                             (concat "\\`" (regexp-quote prefix)
                                     ".*\\.\\(otf\\|ttf\\|woff2?\\)\\'"))))
     agent-shell-dashboard-web-font-directories)))

(defun agent-shell-dashboard-web--font-css ()
  (let ((family (agent-shell-dashboard-web--font-family)))
    (concat
     (mapconcat
      (lambda (file)
        (let ((name (file-name-base file)))
          (format "@font-face { font-family: %S; src: url(\"/font/%s\"); font-weight: %d; font-style: %s; }\n"
                  family
                  (url-hexify-string (file-name-nondirectory file))
                  (cond ((string-match-p "ExtraBold\\|Black\\|Heavy" name) 800)
                        ((string-match-p "SemiBold\\|DemiBold" name) 600)
                        ((string-match-p "Bold" name) 700)
                        ((string-match-p "Medium" name) 500)
                        ((string-match-p "Light" name) 300)
                        (t 400))
                  (if (string-match-p "Italic\\|Oblique" name) "italic" "normal"))))
      (agent-shell-dashboard-web--font-files) "")
     (when family
       (format "html, body, button, input, textarea { font-family: %S, ui-monospace, \"SF Mono\", Menlo, monospace; }\n"
               family)))))

(defun agent-shell-dashboard-web--font-file (name)
  (seq-find (lambda (file) (equal (file-name-nondirectory file) name))
            (agent-shell-dashboard-web--font-files)))

;;;; HTTP

(defun agent-shell-dashboard-web--respond (process status type body &optional cache)
  (let ((bytes (if (multibyte-string-p body) (encode-coding-string body 'utf-8) body)))
    (when (process-live-p process)
      (process-send-string
       process
       (concat (format "HTTP/1.1 %d %s\r\nContent-Type: %s\r\nContent-Length: %d\r\nCache-Control: %s\r\nConnection: close\r\n\r\n"
                       status
                       (pcase status (200 "OK") (401 "Unauthorized") (404 "Not Found")
                              (400 "Bad Request") (_ "Error"))
                       type (length bytes) (if cache "private, max-age=86400" "no-store"))
               bytes))
      (process-send-eof process))))

(defun agent-shell-dashboard-web--json (process status object)
  (agent-shell-dashboard-web--respond
   process status "application/json"
   (if (stringp object) object (json-serialize object))))

(defun agent-shell-dashboard-web--dispatch (process method path query headers body)
  (cond
   ((and (equal method "GET") (string-prefix-p "/font" path))
    (if (equal path "/font.css")
        (agent-shell-dashboard-web--respond
         process 200 "text/css; charset=utf-8"
         (agent-shell-dashboard-web--font-css) t)
      (if-let* ((file (agent-shell-dashboard-web--font-file
                       (url-unhex-string (string-remove-prefix "/font/" path)))))
          (agent-shell-dashboard-web--respond
           process 200
           (pcase (file-name-extension file)
             ("otf" "font/otf") ("ttf" "font/ttf") ("woff" "font/woff") (_ "font/woff2"))
           (with-temp-buffer
             (set-buffer-multibyte nil)
             (insert-file-contents-literally file)
             (buffer-string))
           t)
        (agent-shell-dashboard-web--json process 404 '(:error "no such font")))))

   ((and (equal method "GET") (member path '("/" "/index.html")))
    (agent-shell-dashboard-web--respond
     process 200 "text/html; charset=utf-8"
     (string-replace "__PAGE_VERSION__" (agent-shell-dashboard-web--page-version)
                     (with-temp-buffer
                       (insert-file-contents agent-shell-dashboard-web--page)
                       (buffer-string)))))

   ((not (string-prefix-p "/api/" path))
    (agent-shell-dashboard-web--json process 404 '(:error "not found")))

   (t
    (let ((args (if (s-blank? body)
                    nil
                  (json-parse-string body :object-type 'plist :null-object nil)))
          (buffer nil))
      (if (and (equal path "/api/session")
               (eq (plist-get args :wait) t)
               (setq buffer (agent-shell-dashboard-web--session-buffer (plist-get args :buffer)))
               (equal (agent-shell-dashboard-web--session-state buffer)
                      (list (plist-get args :tick)
                            (eq (plist-get args :busy) t)
                            (plist-get args :queued))))
          (agent-shell-dashboard-web--wait-for-change process buffer args)
      (agent-shell-dashboard-web--json
       process 200
       (pcase path
         ("/api/sessions"
          (when (plist-get args :force)
            (agent-shell-dashboard-web--refresh-closed)
            (setq agent-shell-dashboard-web--snapshot nil))
          (agent-shell-dashboard-web--sessions-json
           (and (not (plist-get args :force)) (plist-get args :etag))))
         ("/api/session"
          (agent-shell-dashboard-web--session
           (agent-shell-dashboard-web--session-buffer (plist-get args :buffer))
           (plist-get args :tick)))
         ("/api/commands"
          (with-current-buffer (agent-shell-dashboard-web--session-buffer
                                (plist-get args :buffer))
            (list :commands
                  (vconcat
                   (seq-map (lambda (cmd)
                              (list :name (or (map-elt cmd 'name) "")
                                    :description (or (map-elt cmd 'description) "")))
                            (map-elt agent-shell--state :available-commands))))))
         ("/api/snippet-commands"
          (let ((text (or (plist-get args :text) "")))
            (list :commands
                  (vconcat (if agent-shell-dashboard-web-console-split-function
                               (funcall agent-shell-dashboard-web-console-split-function text)
                             (list text))))))
         ("/api/console-run"
          (unless agent-shell-dashboard-web-console-run-function
            (signal 'user-error (list "No console configured")))
          (funcall agent-shell-dashboard-web-console-run-function
                   (agent-shell-dashboard-web--session-buffer (plist-get args :buffer))
                   (plist-get args :command))
          (list :ok t))
         ("/api/console"
          (when (and (plist-get args :open) agent-shell-dashboard-web-console-open-function)
            (funcall agent-shell-dashboard-web-console-open-function
                     (agent-shell-dashboard-web--session-buffer (plist-get args :buffer))))
          (list :text
                (if agent-shell-dashboard-web-console-text-function
                    (funcall agent-shell-dashboard-web-console-text-function
                             (agent-shell-dashboard-web--session-buffer (plist-get args :buffer)))
                  "")))
         ("/api/unfork-targets"
          (let ((source (agent-shell-dashboard-web--session-buffer (plist-get args :buffer))))
            (list :targets
                  (vconcat
                   (seq-map (lambda (buffer)
                              (list :buffer (buffer-name buffer)
                                    :summary (or (buffer-local-value
                                                  'agent-shell-dashboard--buffer-summary buffer)
                                                 "")))
                            (agent-shell-dashboard--fork-relatives source))))))
         ("/api/unfork"
          (agent-shell-dashboard--unfork-into
           (agent-shell-dashboard-web--session-buffer (plist-get args :buffer))
           (agent-shell-dashboard-web--session-buffer (plist-get args :target))
           t)
          '(:ok t))
         ("/api/section"
          (agent-shell-dashboard-web--section
           (agent-shell-dashboard-web--session-buffer (plist-get args :buffer))
           (plist-get args :section)))
         ("/api/send"
          (agent-shell-dashboard-web--send
           (agent-shell-dashboard-web--session-buffer (plist-get args :buffer))
           (plist-get args :prompt)))
         ("/api/fork"
          (let ((source (agent-shell-dashboard-web--session-buffer (plist-get args :buffer)))
                (before (agent-shell-dashboard--all-buffers)))
            (unless (agent-shell-dashboard--fork-capable-p source)
              (signal 'user-error (list "This session's agent cannot fork")))
            (with-current-buffer source
              (save-window-excursion (agent-shell-fork)))
            (setq agent-shell-dashboard-web--snapshot nil)
            (let ((fork (car (seq-difference (agent-shell-dashboard--all-buffers) before))))
              (list :buffer (if (buffer-live-p fork) (buffer-name fork) :null)))))
         ("/api/interrupt"
          (with-current-buffer (agent-shell-dashboard-web--session-buffer
                                (plist-get args :buffer))
            (agent-shell-interrupt t))
          '(:ok t))
         ("/api/projects"
          (list :projects (vconcat (agent-shell-dashboard-web--projects))))
         ("/api/new"
          (agent-shell-dashboard-web--new-session (plist-get args :dir)))
         ("/api/resume"
          (agent-shell-dashboard-web--resume (plist-get args :session_id)))
         (_ (signal 'user-error (list (format "Unknown endpoint %s" path)))))))))))

(defun agent-shell-dashboard-web--handle (process request)
  (condition-case err
      (let* ((head-end (string-search "\r\n\r\n" request))
             (lines (split-string (substring request 0 head-end) "\r\n"))
             (request-line (split-string (car lines) " "))
             (headers (delq nil
                            (mapcar (lambda (line)
                                      (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" line)
                                        (cons (downcase (match-string 1 line))
                                              (match-string 2 line))))
                                    (cdr lines))))
             (target (split-string (or (nth 1 request-line) "/") "?"))
             (path (car target))
             (query (url-parse-query-string (or (cadr target) "")))
             (body (decode-coding-string (substring request (+ head-end 4)) 'utf-8)))
        (agent-shell-dashboard-web--dispatch process (car request-line) path query headers body))
    (user-error
     (agent-shell-dashboard-web--json process 400 (list :error (error-message-string err))))
    (error
     (agent-shell-dashboard-web--json process 500 (list :error (error-message-string err))))))

(defun agent-shell-dashboard-web--filter (process chunk)
  (let* ((request (concat (or (process-get process :request) "") chunk))
         (head-end (string-search "\r\n\r\n" request)))
    (process-put process :request request)
    (when head-end
      (let ((length (if (string-match "^[Cc]ontent-[Ll]ength:[ \t]*\\([0-9]+\\)"
                                      (substring request 0 head-end))
                        (string-to-number (match-string 1 (substring request 0 head-end)))
                      0)))
        (when (>= (length request) (+ head-end 4 length))
          (process-put process :request nil)
          (agent-shell-dashboard-web--handle process request))))))

(defun agent-shell-dashboard-web--sentinel (process _event)
  (unless (process-live-p process)
    (delete-process process)))

;;;###autoload
(defun agent-shell-dashboard-web-start ()
  "Serve the dashboard over HTTP and copy its URL."
  (interactive)
  (agent-shell-dashboard-web-stop)
  (setq agent-shell-dashboard-web--maintenance
        (run-with-idle-timer 2 t #'agent-shell-dashboard-web--maintain))
  (setq agent-shell-dashboard-web--server
        (make-network-process
         :name "agent-shell-dashboard-web"
         :server t
         :host agent-shell-dashboard-web-host
         :service agent-shell-dashboard-web-port
         :family 'ipv6
         :coding 'binary
         :noquery t
         :filter #'agent-shell-dashboard-web--filter
         :sentinel #'agent-shell-dashboard-web--sentinel))
  (agent-shell-dashboard-web-url))

(defun agent-shell-dashboard-web-stop ()
  "Stop serving the dashboard over HTTP."
  (interactive)
  (when (process-live-p agent-shell-dashboard-web--server)
    (delete-process agent-shell-dashboard-web--server))
  (when agent-shell-dashboard-web--maintenance
    (cancel-timer agent-shell-dashboard-web--maintenance)
    (setq agent-shell-dashboard-web--maintenance nil))
  (when agent-shell-dashboard-web--waiter-timer
    (cancel-timer agent-shell-dashboard-web--waiter-timer)
    (setq agent-shell-dashboard-web--waiter-timer nil))
  (setq agent-shell-dashboard-web--waiters nil)
  (setq agent-shell-dashboard-web--server nil))

(provide 'agent-shell-dashboard-web)
;;; agent-shell-dashboard-web.el ends here
