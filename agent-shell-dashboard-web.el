;;; agent-shell-dashboard-web.el --- The dashboard in a browser -*- lexical-binding: t; -*-

;; Author: Daniel Gempesaw <gempesaw@gmail.com>

;;; Commentary:

;; Serves the dashboard over HTTP from inside Emacs, so another machine
;; on the network can browse sessions, read a session's buffer and send
;; it prompts.  `agent-shell-dashboard-web-start' starts the server and
;; puts its URL on the kill ring; open that URL once per browser and the
;; page remembers the token it carries.
;;
;; The token matters: prompts sent from the page run with the session's
;; permissions, so anyone who can reach the port without it would be
;; running commands on this machine.  It travels in the URL fragment,
;; which browsers do not send to the server, and the page then sends it
;; in an Authorization header with every request.

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

(defcustom agent-shell-dashboard-web-token-file
  (expand-file-name "agent-shell-dashboard-web-token" user-emacs-directory)
  "File holding the token the web dashboard requires."
  :type 'file
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
(defvar agent-shell-dashboard-web--token nil)
(defvar agent-shell-dashboard-web--snapshot nil)

(defconst agent-shell-dashboard-web--page
  (expand-file-name "agent-shell-dashboard-web.html"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun agent-shell-dashboard-web--token ()
  (or agent-shell-dashboard-web--token
      (setq agent-shell-dashboard-web--token
            (if (file-exists-p agent-shell-dashboard-web-token-file)
                (string-trim (with-temp-buffer
                               (insert-file-contents agent-shell-dashboard-web-token-file)
                               (buffer-string)))
              (let ((token (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (call-process "head" nil t nil "-c" "24" "/dev/urandom")
                             (base64url-encode-string (buffer-string) t))))
                (with-temp-file agent-shell-dashboard-web-token-file
                  (insert token))
                (set-file-modes agent-shell-dashboard-web-token-file #o600)
                token)))))

(defun agent-shell-dashboard-web-url ()
  "Put the web dashboard's URL, token included, on the kill ring."
  (interactive)
  (let ((url (format "http://%s:%d/#%s"
                     (concat (string-trim
                              (shell-command-to-string "scutil --get LocalHostName"))
                             ".local")
                     agent-shell-dashboard-web-port
                     (agent-shell-dashboard-web--token))))
    (kill-new url)
    (message "%s (copied)" url)
    url))

;;;; Serialization

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
          :ago (or (agent-shell-dashboard--format-relative-time time)
                   (if (eq (plist-get row :status) 'closed) "closed" "—"))
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
          :marked (if (agent-shell-dashboard-row-marked-p row) t :false)
          :recency (symbol-name (or (and live (agent-shell-dashboard--row-recency row)) 'none)))))

(defun agent-shell-dashboard-web--sessions ()
  (let* ((rows (agent-shell-dashboard--rows))
         (fork-index (agent-shell-dashboard--compute-fork-index rows))
         (groups (agent-shell-dashboard--group-rows rows))
         (pinned (--keep (assoc it groups) agent-shell-dashboard-pinned-projects)))
    (list :theme (list :bg (agent-shell-dashboard-web--color 'default :background)
                       :fg (agent-shell-dashboard-web--color 'default :foreground)
                       :shadow (agent-shell-dashboard-web--color 'shadow :foreground)
                       :header (agent-shell-dashboard-web--color 'font-lock-function-name-face
                                                                 :foreground))
          :live (length (--filter (plist-get it :buffer) rows))
          :closed (length (--remove (plist-get it :buffer) rows))
          :pinned (vconcat (mapcar #'car pinned))
          :groups (vconcat
                   (mapcar (lambda (group)
                             (list :project (if (s-blank? (car group)) "(none)" (car group))
                                   :rows (vconcat
                                          (mapcar (lambda (row)
                                                    (agent-shell-dashboard-web--row row fork-index))
                                                  (cdr group)))))
                           (append pinned (--remove (memq it pinned) groups)))))))

(defun agent-shell-dashboard-web--sessions-json ()
  (let ((cached agent-shell-dashboard-web--snapshot))
    (if (and cached (< (float-time (time-subtract nil (car cached))) 2))
        (cdr cached)
      (cdr (setq agent-shell-dashboard-web--snapshot
                 (cons (current-time)
                       (json-serialize (agent-shell-dashboard-web--sessions))))))))

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

(defun agent-shell-dashboard-web--runs (start end)
  "Runs of text between START and END as (TEXT STYLE HIDDEN), plus styles."
  (let ((styles (list (list :fg :null :bg :null :bold :false :italic :false :underline :false)))
        (index (make-hash-table :test 'equal))
        runs
        (pos start))
    (while (< pos end)
      (let* ((next (min end (next-char-property-change pos end)))
             (face (or (get-char-property pos 'face)
                       (get-char-property pos 'font-lock-face)))
             (style (if (or (null face) (eq face 'default))
                        0
                      (or (gethash face index)
                          (progn
                            (setq styles (append styles (list (agent-shell-dashboard-web--face-style face))))
                            (puthash face (1- (length styles)) index))))))
        (push (vector (buffer-substring-no-properties pos next)
                      style
                      (if (invisible-p pos) 1 0))
              runs)
        (setq pos next)))
    (cons (vconcat (nreverse runs)) (vconcat styles))))

(defun agent-shell-dashboard-web--session (buffer known-tick)
  (with-current-buffer buffer
    (let ((tick (buffer-modified-tick))
          (busy (if (shell-maker-busy) t :false))
          (queued (vconcat (map-elt agent-shell--state :pending-prompts))))
      (if (equal tick known-tick)
          (list :tick tick :unchanged t :busy busy :queued queued)
        (let* ((start (max (point-min) (- (point-max) agent-shell-dashboard-web-buffer-limit)))
               (runs (agent-shell-dashboard-web--runs start (point-max))))
          (list :tick tick
                :busy busy
                :queued queued
                :truncated (if (> start (point-min)) t :false)
                :summary (or agent-shell-dashboard--buffer-summary :null)
                :runs (car runs)
                :styles (cdr runs)))))))

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

(defun agent-shell-dashboard-web--font-css (token)
  (let ((family (agent-shell-dashboard-web--font-family)))
    (concat
     (mapconcat
      (lambda (file)
        (let ((name (file-name-base file)))
          (format "@font-face { font-family: %S; src: url(\"/font/%s?token=%s\"); font-weight: %d; font-style: %s; }\n"
                  family
                  (url-hexify-string (file-name-nondirectory file))
                  (url-hexify-string token)
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

(defun agent-shell-dashboard-web--authorized-p (headers)
  (let ((given (cdr (assoc "authorization" headers))))
    (and given
         (string= given (concat "Bearer " (agent-shell-dashboard-web--token))))))

(defun agent-shell-dashboard-web--dispatch (process method path query headers body)
  (cond
   ((and (equal method "GET") (string-prefix-p "/font" path))
    (if (not (equal (car (alist-get "token" query nil nil #'equal))
                    (agent-shell-dashboard-web--token)))
        (agent-shell-dashboard-web--json process 401 '(:error "unauthorized"))
      (if (equal path "/font.css")
          (agent-shell-dashboard-web--respond
           process 200 "text/css; charset=utf-8"
           (agent-shell-dashboard-web--font-css (agent-shell-dashboard-web--token)) t)
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
          (agent-shell-dashboard-web--json process 404 '(:error "no such font"))))))

   ((and (equal method "GET") (member path '("/" "/index.html")))
    (agent-shell-dashboard-web--respond
     process 200 "text/html; charset=utf-8"
     (with-temp-buffer
       (insert-file-contents agent-shell-dashboard-web--page)
       (buffer-string))))

   ((not (string-prefix-p "/api/" path))
    (agent-shell-dashboard-web--json process 404 '(:error "not found")))

   ((not (agent-shell-dashboard-web--authorized-p headers))
    (agent-shell-dashboard-web--json process 401 '(:error "unauthorized")))

   (t
    (let ((args (if (s-blank? body)
                    nil
                  (json-parse-string body :object-type 'plist :null-object nil))))
      (agent-shell-dashboard-web--json
       process 200
       (pcase path
         ("/api/sessions" (agent-shell-dashboard-web--sessions-json))
         ("/api/session"
          (agent-shell-dashboard-web--session
           (agent-shell-dashboard-web--session-buffer (plist-get args :buffer))
           (plist-get args :tick)))
         ("/api/send"
          (agent-shell-dashboard-web--send
           (agent-shell-dashboard-web--session-buffer (plist-get args :buffer))
           (plist-get args :prompt)))
         ("/api/interrupt"
          (with-current-buffer (agent-shell-dashboard-web--session-buffer
                                (plist-get args :buffer))
            (agent-shell-interrupt t))
          '(:ok t))
         ("/api/resume"
          (agent-shell-dashboard-web--resume (plist-get args :session_id)))
         (_ (signal 'user-error (list (format "Unknown endpoint %s" path))))))))))

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
  (setq agent-shell-dashboard-web--server nil))

(provide 'agent-shell-dashboard-web)
;;; agent-shell-dashboard-web.el ends here
