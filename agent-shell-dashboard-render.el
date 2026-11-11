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

(defvar-local agent-shell-dashboard--fork-index nil
  "Hash of session-id -> fork-group index for the current render.
Only session-ids with two or more live buffers (i.e. forks sharing an
id) appear.  Rebuilt each `agent-shell-dashboard-refresh'.")

(defcustom agent-shell-dashboard-fork-colors
  '("#ff6ac1" "#57c7ff" "#5af78e" "#ffb86c" "#bd93f9" "#ff5555" "#8be9fd")
  "Foreground colors cycled across distinct fork groups.
Each group of forked sessions gets one color; both members of a pair
share it, so two independent forks never look like one group of four."
  :type '(repeat color)
  :group 'agent-shell-dashboard)

;; Row tints all sit in the violet -> magenta arc and differ by hue and
;; lift rather than by going dark.  On fairyfloss's #5A5475 background a
;; darker tint just reads as mud (dark gold over purple turns brown), so
;; the "you" states lean warm/magenta, the machine state leans cool
;; indigo, and staleness is the only thing allowed to recede.

(defface agent-shell-dashboard-awaiting-row-face
  '((((background dark))  :background "#7d3a6a")
    (((background light)) :background "#ffd9ef"))
  "Persistent row background for sessions whose agent finished
recently and are waiting for the user's next prompt.
Hottest tint in the palette — this is the state that actually wants
the user, so it gets the strongest rose."
  :group 'agent-shell-dashboard)

(defface agent-shell-dashboard-recent-row-face
  '((((background dark))  :background "#6b3f7d")
    (((background light)) :background "#f3e0ff"))
  "Persistent row background for idle sessions touched within
`agent-shell-dashboard-recent-interaction-seconds'.  Orchid: one clear
step down from the rose of `your turn'."
  :group 'agent-shell-dashboard)

(defface agent-shell-dashboard-today-row-face
  '((((background dark))  :background "#645a86")
    (((background light)) :background "#efeaff"))
  "Persistent row background for live sessions touched earlier today.
A gentle lift off the background — still in play, but past the
recent-interaction window."
  :group 'agent-shell-dashboard)

(defface agent-shell-dashboard-stale-row-face
  '((((background dark))  :background "#4e485f")
    (((background light)) :background "#eceaf0"))
  "Persistent row background for live sessions last touched before today.
Desaturated and recessed, so yesterday's threads sink behind today's."
  :group 'agent-shell-dashboard)

(defface agent-shell-dashboard-working-row-face
  '((((background dark))  :background "#3f3a6e")
    (((background light)) :background "#e2e4ff"))
  "Persistent row background for sessions currently working.
Cool indigo, off the warm axis entirely: the agent is busy and this is
the one state that is not about the user."
  :group 'agent-shell-dashboard)

(defface agent-shell-dashboard-permission-row-face
  '((((background dark))  :background "#9c3350")
    (((background light)) :background "#ffd2d9"))
  "Persistent row background for sessions awaiting a permission decision.
Dormant unless `agent-shell-dashboard--buffer-status' is overridden to
detect pending permission requests, which the public package does not
do.  Kept distinct from the awaiting rose so the two never collide if
that detection is ever wired up."
  :group 'agent-shell-dashboard)

(defcustom agent-shell-dashboard-recent-interaction-seconds 3600
  "Age in seconds under which a session counts as recently interacted with.
Rows younger than this get `agent-shell-dashboard-recent-row-face'."
  :type 'integer
  :group 'agent-shell-dashboard)

(defun agent-shell-dashboard--row-recency (row)
  "Return `recent', `today', or `older' for ROW's last activity, or nil.
`recent' is within `agent-shell-dashboard-recent-interaction-seconds';
`today' is the rest of the current calendar day; `older' is anything
before that."
  (when-let* ((time (agent-shell-dashboard--row-time row)))
    (cond
     ((< (float-time (time-subtract (current-time) time))
         agent-shell-dashboard-recent-interaction-seconds)
      'recent)
     ((agent-shell-dashboard--row-is-today row) 'today)
     (t 'older))))

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

(defun agent-shell-dashboard--row-face (row)
  "Background face for ROW, or nil when it stays untinted.
Blocking and busy states always win.  `awaiting' only tints while it is
still fresh: it fires for any idle session with unread history, so on a
busy dashboard nearly every row qualifies and the tint stops
discriminating.  Closed rows stay untinted; they already sort last and
carry a shadowed glyph."
  (let ((recency (and (plist-get row :buffer)
                      (agent-shell-dashboard--row-recency row)))
        (status (plist-get row :status)))
    (cond
     ((eq status 'permission) 'agent-shell-dashboard-permission-row-face)
     ((eq status 'working)    'agent-shell-dashboard-working-row-face)
     ((and (eq status 'awaiting) (eq recency 'recent))
      'agent-shell-dashboard-awaiting-row-face)
     (t (pcase recency
          ('recent 'agent-shell-dashboard-recent-row-face)
          ('today  'agent-shell-dashboard-today-row-face)
          ('older  'agent-shell-dashboard-stale-row-face))))))

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
             (face (and row (agent-shell-dashboard--row-face row))))
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
    ('ready      (propertize "●" 'face '(:foreground "#c5a3ff")))
    ('awaiting   (propertize "◉" 'face '(:foreground "#ffb8d1" :weight bold)))
    ('working    (propertize "▶" 'face '(:foreground "#a8b6ff" :weight bold)))
    ('permission (propertize "!" 'face '(:foreground "#ff8fc8" :weight bold)))
    ('closed     (propertize "○" 'face (if is-today
                                           '(:foreground "#b8a2ce")
                                         'shadow)))
    (_           (propertize "·" 'face 'shadow))))

(defun agent-shell-dashboard--compute-fork-index (rows)
  "Return a hash of session-id -> fork-group index for ROWS.
Rows group by the session they descend from, so a parent and the
sessions forked off it share a badge.  A group is badged once it has
two or more rows and at least one is live."
  (agent-shell-dashboard--record-fork-lineage)
  (let ((groups (ht-create)))
    (dolist (row rows)
      (when-let* ((id (plist-get row :session-id)))
        (let ((root (agent-shell-dashboard--fork-root id)))
          (ht-set! groups root (cons row (ht-get groups root))))))
    (let ((idx (ht-create))
          (roots (sort (--filter (let ((members (ht-get groups it)))
                                   (and (cdr members)
                                        (--some (plist-get it :buffer) members)))
                                 (ht-keys groups))
                       #'string<)))
      (--each-indexed roots
        (let ((i it-index))
          (dolist (row (ht-get groups it))
            (ht-set! idx (plist-get row :session-id) i))))
      idx)))

(defun agent-shell-dashboard--fork-badge (index)
  "Return a colored badge string for fork-group INDEX, or nil.
The fork glyph plus a per-group letter; the letter disambiguates
groups even where colors are hard to tell apart."
  (when index
    (let ((color (nth (mod index (length agent-shell-dashboard-fork-colors))
                      agent-shell-dashboard-fork-colors)))
      (propertize (format "⑂%c " (+ ?a (mod index 26)))
                  'face (list :foreground color :weight 'bold)))))

(defun agent-shell-dashboard--row-cells (row width)
  "Return two cells (line1 line2) representing ROW, each WIDTH-wide.
Each cell is a plist with :string and :row keys."
  (let* ((status (plist-get row :status))
         (is-today (agent-shell-dashboard--row-is-today row))
         (glyph (agent-shell-dashboard--status-glyph status is-today))
         (activity (or (agent-shell-dashboard--format-relative-time
                        (agent-shell-dashboard--row-time row))
                       (if (eq status 'closed) "closed" "—")))
         (fork-badge (agent-shell-dashboard--fork-badge
                      (and agent-shell-dashboard--fork-index
                           (ht-get agent-shell-dashboard--fork-index
                                   (plist-get row :session-id)))))
         (summary (concat
                   fork-badge
                   (agent-shell-dashboard--collapse-whitespace
                    (agent-shell-dashboard--clean-summary
                     (plist-get row :summary)))))
         (preview (agent-shell-dashboard--collapse-whitespace
                   (plist-get row :last-prompt-text)))
         ;; The mark rides in the leading gutter rather than in front of
         ;; the summary, so marking a row never reflows its text.
         (mark (if (agent-shell-dashboard-row-marked-p row)
                   (propertize "*" 'face '(:foreground "#ffb8d1" :weight bold))
                 " "))
         (line1 (format "%s %s  %-7s  %s"
                        mark
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
         (_ (setq agent-shell-dashboard--fork-index
                  (agent-shell-dashboard--compute-fork-index rows)))
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

(defun agent-shell-dashboard--jump-label-lines (labeled)
  "Dashboard line numbers LABELED puts something on, sorted."
  (->> labeled
       (--mapcat (list (plist-get (cdr it) :header-pos)
                       (plist-get (cdr it) :first-row-pos)))
       (-non-nil)
       (-map #'line-number-at-pos)
       (-distinct)
       (-sort #'<)))

(defun agent-shell-dashboard--jump-label-height (labeled)
  "Scale for LABELED's letters, shrunk when they would not all fit.
Each letter costs roughly its own height in screen rows, so enough
bands of projects overflow the window even once the gaps are collapsed.
Budgets by distinct label lines rather than by column, since every
column in a band shares one line."
  (let* ((distinct (lambda (key)
                     (->> labeled
                          (--keep (plist-get (cdr it) key))
                          (-map #'line-number-at-pos)
                          (-distinct)
                          (length))))
         (rows (funcall distinct :first-row-pos))
         (headers (funcall distinct :header-pos))
         (budget (- (window-body-height) (* headers 1.5) 4)))
    (if (or (zerop rows) (<= budget 0))
        5.0
      (max 1.5 (min 5.0 (/ budget (float rows)))))))

(defun agent-shell-dashboard--insert-jump-positional (lines heights first last)
  "Insert LINES FIRST..LAST, each tall row absorbing the blanks beneath it.
A letter drawn at five times the normal height occupies five screen
rows, so it is emitted in place of the five dashboard rows it covers
rather than on top of them.  Everything below therefore keeps the row
it had, which is the whole point of the view: the eye goes where the
column already was."
  (let ((row first))
    (while (<= row last)
      (insert (ht-get lines row "") "\n")
      (let* ((span (max 1 (ceiling (ht-get heights row 1))))
             (limit (+ row span)))
        ;; Never let a tall row swallow one that carries its own label.
        (setq row (or (cl-loop for probe from (1+ row) below limit
                               when (ht-contains? lines probe) return probe)
                      limit))))))

(defun agent-shell-dashboard--insert-jump-collapsed (lines keys)
  "Insert the KEYS of LINES, collapsing blank runs to one separator.
Loses the dashboard's vertical spacing, so it is the fallback for
layouts too tall to reproduce — a label in roughly the right place
beats one scrolled off the screen."
  (let ((previous nil))
    (dolist (line keys)
      (when (and previous (> line (1+ previous)))
        (insert "\n"))
      (insert (ht-get lines line "") "\n")
      (setq previous line))))

(defun agent-shell-dashboard--render-jump-view (labeled)
  "Erase buffer and render only project headers with giant labels.
Letters keep the column and, where it fits, the row the dashboard
showed each project at, so the eye lands where it already was.

Reproducing the dashboard line for line is what broke this before: that
buffer runs to hundreds of lines against a window of tens, so the view
scrolled and the first band of projects sat above the top edge.  Only
the span actually carrying labels is reproduced, and the enlarged
letters stand in for the blank rows they cover instead of displacing
them.  When even that span is taller than the window, fall back to
collapsing the gaps."
  (let* ((inhibit-read-only t)
         (label-lines (agent-shell-dashboard--jump-label-lines labeled))
         (last (car (last label-lines)))
         ;; Carried through verbatim, and the walk starts at line 1 rather
         ;; than at the first label: dropping the dashboard's own header
         ;; row shifts every column up by it.
         (banner (save-excursion
                   (goto-char (point-min))
                   (buffer-substring (line-beginning-position)
                                     (line-end-position))))
         ;; The last letter absorbs rows past the span it was measured
         ;; over, so leave its height as headroom before claiming a fit.
         (positional (and last (<= (+ 5 last) (window-body-height))))
         (label-height (if positional
                           5.0
                         (agent-shell-dashboard--jump-label-height labeled)))
         (lines (ht-create))
         (heights (ht-create)))
    (cl-flet ((place (pos str height)
                (let ((line (line-number-at-pos pos))
                      (col (save-excursion (goto-char pos) (current-column))))
                  (ht-set! lines line
                           (agent-shell-dashboard--place-at-col
                            (ht-get lines line "") col str))
                  (ht-set! heights line (max height (ht-get heights line 1))))))
      (dolist (pair labeled)
        (let* ((ch (car pair))
               (d (cdr pair))
               (h-pos (plist-get d :header-pos))
               (fr-pos (plist-get d :first-row-pos)))
          (when h-pos
            (place h-pos
                   (propertize (or (plist-get d :project) "")
                               'face '(:inherit font-lock-function-name-face
                                       :height 1.5 :weight bold))
                   1.5))
          (when fr-pos
            (place fr-pos
                   (propertize (format " %c " ch)
                               'face `(:foreground "black"
                                       :background "yellow"
                                       :weight bold
                                       :height ,label-height))
                   label-height)))))
    (unless (ht-contains? lines 1)
      (ht-set! lines 1 banner))
    (erase-buffer)
    (when label-lines
      (if positional
          (agent-shell-dashboard--insert-jump-positional lines heights 1 last)
        (agent-shell-dashboard--insert-jump-collapsed
         lines (-sort #'< (ht-keys lines)))))
    (goto-char (point-min))
    (when-let ((window (get-buffer-window (current-buffer))))
      (set-window-start window (point-min)))))

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
