;;; agent-shell-dashboard-render.el --- Rendering and visual state -*- lexical-binding: t; -*-

;;; Commentary:

;; Cell construction, column layout, status overlays, the main
;; `agent-shell-dashboard-refresh' entry point, and the
;; switch-window-style jump view.  Reads rows from the data module
;; and paints them into the dashboard buffer.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 's)
(require 'agent-shell-dashboard-data)

(defvar-local agent-shell-dashboard--row-positions nil
  "Buffer positions of rendered rows, in display order.")

(defvar-local agent-shell-dashboard--columns nil
  "Per-column metadata: list of plists with :header-pos and :first-row-pos.")

(defvar-local agent-shell-dashboard--highlight-overlays nil
  "Overlays highlighting the row at point, refreshed in `post-command-hook'.")

(defvar-local agent-shell-dashboard--row-status-overlays nil
  "Persistent overlays applied to working / permission rows on render.")

(defface agent-shell-dashboard-working-row-face
  '((((background dark))  :background "#1f2d3d")
    (((background light)) :background "#e7eef6"))
  "Persistent row background for sessions currently working."
  :group 'agent-shell-dashboard)

(defface agent-shell-dashboard-permission-row-face
  '((((background dark))  :background "#3a2a1a")
    (((background light)) :background "#f4ece0"))
  "Persistent row background for sessions awaiting a permission decision."
  :group 'agent-shell-dashboard)

(defface agent-shell-dashboard-awaiting-row-face
  '((((background dark))  :background "#5e5028")
    (((background light)) :background "#fef9b8"))
  "Persistent row background for sessions whose agent finished
recently and are waiting for the user's next prompt.
Gold-tinted to read as `your turn' on the fairyfloss palette."
  :group 'agent-shell-dashboard)

(defcustom agent-shell-dashboard-jump-keys
  "asdfjkl;qwertyuiopzxcvbnm"
  "Letters used as single-key shortcuts in `agent-shell-dashboard-jump'."
  :type 'string
  :group 'agent-shell-dashboard)

(defcustom agent-shell-dashboard-column-width 80
  "Width in characters of each project column in the dashboard."
  :type 'integer
  :group 'agent-shell-dashboard)

(defcustom agent-shell-dashboard-column-gap 2
  "Number of blank chars between adjacent project columns."
  :type 'integer
  :group 'agent-shell-dashboard)

(defcustom agent-shell-dashboard-pinned-projects '("infra")
  "Project names rendered as dedicated full-height leftmost columns.
Pinned columns appear in this list's order, left to right.  All
remaining projects flow into right-side column bands.  An empty
list reverts to the uniform banded layout across all projects."
  :type '(repeat string)
  :group 'agent-shell-dashboard)

(defun agent-shell-dashboard--clear-highlight ()
  "Remove the row-highlight overlays."
  (-each agent-shell-dashboard--highlight-overlays #'delete-overlay)
  (setq agent-shell-dashboard--highlight-overlays nil))

(defun agent-shell-dashboard--apply-row-status-overlays ()
  "Tint working / permission rows with a persistent background.
Cursor's hl-line overlay has higher priority and so wins on the
row at point."
  (-each agent-shell-dashboard--row-status-overlays #'delete-overlay)
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
  (when-let* ((row (get-text-property (point) 'dg-row)))
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
         (preview (s-replace-regexp "[ \t\n\r]+" " "
                                    (or (plist-get row :last-prompt-text) "")))
         (line1 (format "  %s  %-7s  %s"
                        glyph
                        (propertize activity 'face 'shadow)
                        summary))
         (line2 (concat "        "
                        (propertize "> " 'face 'shadow)
                        (propertize (if (s-blank? preview) "—" preview)
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
         (n-live (--count (plist-get it :buffer) rows))
         (n-closed (- (length rows) n-live))
         (header (concat
                  (propertize (if (s-blank? project) "(none)" project)
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
  (let* ((columns (--map (agent-shell-dashboard--group-cells it width) groups))
         (height (apply #'max (-map #'length columns)))
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
         (pinned-columns (--map (agent-shell-dashboard--group-cells it width)
                                pinned-groups))
         (n-pinned (length pinned-columns))
         (left-block-width (if (zerop n-pinned)
                               0
                             (+ (* width n-pinned) (* gap n-pinned))))
         (right-area-width (max 80 (- (max 80 (frame-width)) left-block-width)))
         (cols-per-band (max 1 (/ right-area-width (+ width gap))))
         (right-bands (-partition-all cols-per-band right-groups))
         (right-lines nil)
         (band-idx 0))
    (dolist (band right-bands)
      (let* ((cols (--map (agent-shell-dashboard--group-cells it width) band))
             (band-h (apply #'max (-map #'length cols))))
        (dotimes (line-idx band-h)
          (push (list :type 'data
                      :band-idx band-idx
                      :line-in-band line-idx
                      :cells (--map (or (nth line-idx it) blank-cell) cols))
                right-lines))
        (push (list :type 'spacer) right-lines))
      (cl-incf band-idx))
    (setq right-lines (nreverse right-lines))
    (let* ((max-pinned-h (if (zerop n-pinned)
                             0
                           (apply #'max (-map #'length pinned-columns))))
           (total (max max-pinned-h (length right-lines)))
           ;; Per-pinned-column tracking: vectors indexed by pinned col idx.
           (pinned-headers (make-vector n-pinned nil))
           (pinned-firsts  (make-vector n-pinned nil))
           ;; Right-side tracking, keyed by (band-idx . col-idx).
           (right-tracker (ht-create)))
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
                  (let ((entry (ht-get right-tracker key)))
                    (when (= line-in-band 0)
                      (ht-set! right-tracker key
                               (cons right-cell-start (cdr entry))))
                    (when (and (plist-get cell :row-start)
                               (or (not entry) (not (cdr entry))))
                      (ht-set! right-tracker key
                               (cons (or (car (ht-get right-tracker key))
                                         right-cell-start)
                                     right-cell-start)))))
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
          (let ((entry (ht-get right-tracker (cons b c))))
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
         (live (--filter (plist-get it :buffer) rows))
         (closed (--remove (plist-get it :buffer) rows))
         (groups (agent-shell-dashboard--group-rows rows))
         (width agent-shell-dashboard-column-width)
         (gap agent-shell-dashboard-column-gap)
         (pinned-names agent-shell-dashboard-pinned-projects)
         ;; Resolve pinned project names to groups, preserving the requested order.
         (pinned-groups (--keep (assoc it groups) pinned-names))
         (effective-groups
          (if pinned-groups
              (--remove (memq it pinned-groups) groups)
            groups))
         (cols-per-band (max 1 (/ (max 80 (frame-width)) (+ width gap))))
         (bands (-partition-all cols-per-band effective-groups)))
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

(defun agent-shell-dashboard--render-jump-view (labeled)
  "Erase buffer and render only project headers with giant labels.
Letters are placed at the same horizontal column as the dashboard
showed each project, so the eye lands in the right place."
  (let* ((inhibit-read-only t)
         (lines (ht-create)))
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
                 (existing (ht-get lines line "")))
            (ht-set! lines line
                     (agent-shell-dashboard--place-at-col
                      existing col
                      (propertize (or project "")
                                  'face '(:inherit font-lock-function-name-face
                                          :height 1.5 :weight bold))))))
        (when fr-pos
          (let* ((line (line-number-at-pos fr-pos))
                 (col (save-excursion (goto-char fr-pos) (current-column)))
                 (existing (ht-get lines line "")))
            (ht-set! lines line
                     (agent-shell-dashboard--place-at-col
                      existing col
                      (propertize (format " %c " ch)
                                  'face '(:foreground "black"
                                          :background "yellow"
                                          :weight bold
                                          :height 5.0))))))))
    (erase-buffer)
    ;; Take over the dashboard's natural lines 1-2 (its own header +
    ;; blank) so the project-header line stays at the same buffer line
    ;; as in normal view — otherwise we'd inject extra blank lines.
    (insert (propertize "Pick a column:" 'face 'shadow) "\n\n")
    (let* ((line-keys (sort (ht-keys lines) #'<))
           (max-line (or (car (last line-keys)) 0)))
      (cl-loop for i from 3 to max-line do
               (insert (ht-get lines i "") "\n")))))

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
  (let* ((targets (--filter (plist-get it :first-row-pos)
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
          (let* ((new-targets (--filter (plist-get it :first-row-pos)
                                        agent-shell-dashboard--columns))
                 (tgt (nth chosen-idx new-targets)))
            (when tgt (goto-char (plist-get tgt :first-row-pos)))))
         (t (goto-char (min saved-point (point-max)))))))))

(provide 'agent-shell-dashboard-render)
;;; agent-shell-dashboard-render.el ends here
