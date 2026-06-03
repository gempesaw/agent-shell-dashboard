;;; agent-shell-dashboard-test.el --- Tests -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for agent-shell-dashboard.  Run from the repo root with:
;;
;;   make test
;;
;; Coverage skews toward pure functions (sorting, grouping, time
;; formatting, glyph selection, jump-view string positioning).  A few
;; integration tests cover the persistence file readers/writers and
;; the JSONL filesystem enumerator using a temp fixture tree.

;;; Code:

(require 'ert)
(require 'agent-shell-dashboard)


;;;; --- Pure helpers --------------------------------------------------

(ert-deftest asd-test-format-relative-time-buckets ()
  "Each bucket boundary picks the next unit up."
  (let ((now (current-time)))
    (should (s-ends-with? "s ago"
                          (agent-shell-dashboard--format-relative-time
                           (time-subtract now 30))))
    (should (s-ends-with? "m ago"
                          (agent-shell-dashboard--format-relative-time
                           (time-subtract now 600))))
    (should (s-ends-with? "h ago"
                          (agent-shell-dashboard--format-relative-time
                           (time-subtract now (* 2 3600)))))
    (should (s-ends-with? "d ago"
                          (agent-shell-dashboard--format-relative-time
                           (time-subtract now (* 3 86400))))))
  (should-not (agent-shell-dashboard--format-relative-time nil)))

(ert-deftest asd-test-row-time-prefers-prompt ()
  "`:last-prompt-time' wins over `:updated-at' string."
  (let* ((now (current-time))
         (row (list :last-prompt-time now
                    :updated-at "2020-01-01T00:00:00+0000")))
    (should (equal now (agent-shell-dashboard--row-time row))))
  (let ((row (list :updated-at "2020-01-01T00:00:00+0000")))
    (should (agent-shell-dashboard--row-time row)))
  (should-not (agent-shell-dashboard--row-time '(:foo bar))))

(ert-deftest asd-test-row-less-p-live-before-closed ()
  "A row with a buffer sorts ahead of a row without."
  (should (agent-shell-dashboard--row-less-p
           '(:buffer t  :last-prompt-time nil)
           '(:buffer nil :last-prompt-time nil))))

(ert-deftest asd-test-row-less-p-newer-before-older ()
  "Within live rows, newer activity sorts first."
  (let ((newer (current-time))
        (older (time-subtract (current-time) 3600)))
    (should (agent-shell-dashboard--row-less-p
             (list :buffer t :last-prompt-time newer)
             (list :buffer t :last-prompt-time older)))
    (should-not (agent-shell-dashboard--row-less-p
                 (list :buffer t :last-prompt-time older)
                 (list :buffer t :last-prompt-time newer)))))

(ert-deftest asd-test-row-is-today ()
  "Today's date matches; tomorrow's wouldn't."
  (should (agent-shell-dashboard--row-is-today
           (list :last-prompt-time (current-time))))
  (should-not (agent-shell-dashboard--row-is-today
               (list :last-prompt-time
                     (time-subtract (current-time) (* 2 86400))))))

(ert-deftest asd-test-row-project-basename ()
  "`:cwd' is reduced to its basename even when trailing slash present."
  (should (s-equals? "infra"
                     (agent-shell-dashboard--row-project '(:cwd "/Users/x/opt/infra"))))
  (should (s-equals? "infra"
                     (agent-shell-dashboard--row-project '(:cwd "/Users/x/opt/infra/"))))
  (should (s-equals? ""
                     (agent-shell-dashboard--row-project '(:cwd nil))))
  (should (s-equals? "" (agent-shell-dashboard--row-project '()))))


;;;; --- Grouping ------------------------------------------------------

(ert-deftest asd-test-group-rows-buckets-by-project ()
  "Rows under the same cwd basename go in one bucket."
  (let* ((rows (list (list :cwd "/a/b/infra"    :buffer t)
                     (list :cwd "/a/b/infra"    :buffer nil)
                     (list :cwd "/a/b/dotemacs" :buffer t)))
         (groups (agent-shell-dashboard--group-rows rows)))
    (should (= 2 (length groups)))
    (let ((infra (assoc "infra" groups)))
      (should infra)
      (should (= 2 (length (cdr infra)))))))

(ert-deftest asd-test-group-less-p-live-first ()
  "A group with any live row sorts before an all-closed group."
  (let ((g-live   (cons "a" (list (list :buffer t))))
        (g-closed (cons "b" (list (list :buffer nil)))))
    (should      (agent-shell-dashboard--group-less-p g-live g-closed))
    (should-not  (agent-shell-dashboard--group-less-p g-closed g-live))))

(ert-deftest asd-test-group-newest-time ()
  "Returns the newest time across rows."
  (let* ((older  (time-subtract (current-time) 3600))
         (newer  (current-time))
         (rows   (list (list :last-prompt-time older)
                       (list :last-prompt-time newer)
                       '(:last-prompt-time nil))))
    (should (equal newer
                   (agent-shell-dashboard--group-newest-time rows))))
  (should-not (agent-shell-dashboard--group-newest-time
               (list '(:no-time-here t)))))


;;;; --- Status glyphs -------------------------------------------------

(ert-deftest asd-test-status-glyph-shape ()
  "Each status returns a known single-character glyph."
  (should (s-equals? "●" (substring-no-properties
                          (agent-shell-dashboard--status-glyph 'ready))))
  (should (s-equals? "◉" (substring-no-properties
                          (agent-shell-dashboard--status-glyph 'awaiting))))
  (should (s-equals? "▶" (substring-no-properties
                          (agent-shell-dashboard--status-glyph 'working))))
  (should (s-equals? "!" (substring-no-properties
                          (agent-shell-dashboard--status-glyph 'permission))))
  (should (s-equals? "○" (substring-no-properties
                          (agent-shell-dashboard--status-glyph 'closed))))
  (should (s-equals? "·" (substring-no-properties
                          (agent-shell-dashboard--status-glyph 'mystery)))))


;;;; --- Tracking helper ----------------------------------------------

(ert-deftest asd-test-status-line-p ()
  "Status-spam lines are filtered from summary capture."
  (should (agent-shell-dashboard--status-line-p ""))
  (should (agent-shell-dashboard--status-line-p "▶ done foo"))
  (should (agent-shell-dashboard--status-line-p "Done"))
  (should (agent-shell-dashboard--status-line-p "Requesting model"))
  (should (agent-shell-dashboard--status-line-p "<shell-maker-end-of-prompt>"))
  (should-not (agent-shell-dashboard--status-line-p "an actual summary")))


;;;; --- Jump view string surgery -------------------------------------

(ert-deftest asd-test-place-at-col-pads-when-short ()
  "Placing past the end pads with spaces."
  (should (s-equals? "abc   X"
                     (agent-shell-dashboard--place-at-col "abc" 6 "X"))))

(ert-deftest asd-test-place-at-col-overwrites-middle ()
  "Placing within the existing string overwrites without truncating."
  (should (s-equals? "abXYef"
                     (agent-shell-dashboard--place-at-col "abcdef" 2 "XY"))))

(ert-deftest asd-test-place-at-col-overwrites-past-end ()
  "Placing late and long extends the line cleanly."
  (should (s-equals? "abcdeXYZ"
                     (agent-shell-dashboard--place-at-col "abcdef" 5 "XYZ"))))


;;;; --- Persistence file I/O round-trip ------------------------------

(ert-deftest asd-test-active-sessions-write-then-read ()
  "Write/read round-trip preserves entries."
  (let ((tmp (make-temp-file "asd-active-" nil ".el")))
    (unwind-protect
        (let ((agent-shell-dashboard-active-sessions-file tmp)
              (entries '((:session-id "abc" :cwd "/tmp" :identifier claude-code)
                         (:session-id "def" :cwd "/var" :identifier claude-code))))
          (agent-shell-dashboard--write-active-sessions entries)
          (should (equal entries
                         (agent-shell-dashboard--read-active-sessions))))
      (delete-file tmp))))

(ert-deftest asd-test-summary-archive-write-then-read ()
  "Archive round-trip preserves summary plists."
  (let ((tmp (make-temp-file "asd-summary-" nil ".el")))
    (unwind-protect
        (let ((agent-shell-dashboard-summary-archive-file tmp)
              (entries '((:session-id "abc" :summary "foo" :cwd "/tmp")
                         (:session-id "def" :summary "bar" :cwd "/var"))))
          (agent-shell-dashboard--write-summary-archive entries)
          (should (equal entries
                         (agent-shell-dashboard--read-summary-archive))))
      (delete-file tmp))))

(ert-deftest asd-test-forget-session-removes-from-both ()
  "Forgetting drops the session from active + archive files."
  (let ((active-tmp  (make-temp-file "asd-active-"  nil ".el"))
        (archive-tmp (make-temp-file "asd-summary-" nil ".el")))
    (unwind-protect
        (let ((agent-shell-dashboard-active-sessions-file active-tmp)
              (agent-shell-dashboard-summary-archive-file archive-tmp))
          (agent-shell-dashboard--write-active-sessions
           '((:session-id "keep") (:session-id "drop")))
          (agent-shell-dashboard--write-summary-archive
           '((:session-id "keep" :summary "a")
             (:session-id "drop" :summary "b")))
          (agent-shell-dashboard--forget-session "drop")
          (should (equal '((:session-id "keep"))
                         (agent-shell-dashboard--read-active-sessions)))
          (should (equal '((:session-id "keep" :summary "a"))
                         (agent-shell-dashboard--read-summary-archive))))
      (delete-file active-tmp)
      (delete-file archive-tmp))))


;;;; --- JSONL enumeration --------------------------------------------

(defun asd-test--make-jsonl-fixture (dir project-name session-id)
  "Create a Claude-shaped JSONL under DIR for PROJECT-NAME and SESSION-ID."
  (let* ((cwd (expand-file-name project-name "/Users/test/opt"))
         (encoded (s-replace "/" "-" (directory-file-name cwd)))
         (proj-dir (expand-file-name encoded dir))
         (file (expand-file-name (concat session-id ".jsonl") proj-dir)))
    (make-directory proj-dir t)
    (with-temp-file file
      (insert
       (format
        "{\"type\":\"queue-operation\",\"sessionId\":\"%s\"}\n"
        session-id))
      (insert
       (format
        "{\"type\":\"user\",\"cwd\":\"%s\",\"sessionId\":\"%s\"}\n"
        cwd session-id)))
    file))

(ert-deftest asd-test-sniff-jsonl-cwd ()
  "cwd is recovered from the first JSONL line that contains it."
  (let* ((fixtures (make-temp-file "asd-jsonl-" t))
         (file (asd-test--make-jsonl-fixture fixtures "infra" "sess-1")))
    (unwind-protect
        (should (s-equals? "/Users/test/opt/infra"
                           (agent-shell-dashboard--sniff-jsonl-cwd file)))
      (delete-directory fixtures t))))

(ert-deftest asd-test-enumerate-jsonls-shape ()
  "Enumeration returns one entry per JSONL with cwd + session id."
  (let ((fixtures (make-temp-file "asd-jsonl-" t)))
    (unwind-protect
        (let ((agent-shell-dashboard-claude-projects-dir fixtures))
          (asd-test--make-jsonl-fixture fixtures "infra"    "sess-1")
          (asd-test--make-jsonl-fixture fixtures "dotemacs" "sess-2")
          (let ((entries (agent-shell-dashboard--enumerate-jsonls)))
            (should (= 2 (length entries)))
            (should (--all? (and (plist-get it :session-id)
                                 (plist-get it :cwd)
                                 (eq 'claude-code (plist-get it :identifier)))
                            entries))))
      (delete-directory fixtures t))))


;;;; --- Multi-source row merging -------------------------------------

(ert-deftest asd-test-rows-merges-and-dedupes ()
  "Live > active-file > archive > jsonl; session ids appear once."
  (let ((active-tmp  (make-temp-file "asd-active-"  nil ".el"))
        (archive-tmp (make-temp-file "asd-summary-" nil ".el"))
        (jsonl-root  (make-temp-file "asd-jsonl-"   t)))
    (unwind-protect
        (let ((agent-shell-dashboard-active-sessions-file active-tmp)
              (agent-shell-dashboard-summary-archive-file archive-tmp)
              (agent-shell-dashboard-claude-projects-dir jsonl-root))
          (agent-shell-dashboard--write-active-sessions
           '((:session-id "in-active" :cwd "/x" :identifier claude-code)))
          (agent-shell-dashboard--write-summary-archive
           '((:session-id "in-active"  :summary "summary from archive")
             (:session-id "in-archive" :summary "archive only"
                          :cwd "/y" :updated-at "2024-01-01T00:00:00+0000")))
          (asd-test--make-jsonl-fixture jsonl-root "z" "in-jsonl")
          (let* ((rows (agent-shell-dashboard--rows))
                 (ids (--map (plist-get it :session-id) rows)))
            (should (= 3 (length rows)))
            (should (member "in-active"  ids))
            (should (member "in-archive" ids))
            (should (member "in-jsonl"   ids))
            ;; backfill: in-active had no summary, archive supplies one
            (let ((entry (--find (s-equals? "in-active" (plist-get it :session-id))
                                 rows)))
              (should (s-equals? "summary from archive"
                                 (plist-get entry :summary))))))
      (delete-file active-tmp)
      (delete-file archive-tmp)
      (delete-directory jsonl-root t))))

;;;; --- Tombstones -----------------------------------------------

(ert-deftest asd-test-tombstone-roundtrip ()
  "Tombstoning then untombstoning is symmetric."
  (let ((tmp (make-temp-file "asd-forgotten-" nil ".el")))
    (unwind-protect
        (let ((agent-shell-dashboard-forgotten-ids-file tmp))
          (agent-shell-dashboard--tombstone-session "abc")
          (agent-shell-dashboard--tombstone-session "def")
          (agent-shell-dashboard--tombstone-session "abc")  ;; idempotent
          (should (equal '("def" "abc")
                         (agent-shell-dashboard--read-forgotten-ids)))
          (agent-shell-dashboard--untombstone-session "abc")
          (should (equal '("def")
                         (agent-shell-dashboard--read-forgotten-ids))))
      (delete-file tmp))))

(ert-deftest asd-test-rows-filters-tombstoned-jsonls ()
  "A tombstoned session id is hidden from the JSONL source."
  (let ((forgotten-tmp (make-temp-file "asd-forgotten-" nil ".el"))
        (jsonl-root    (make-temp-file "asd-jsonl-"     t)))
    (unwind-protect
        (let ((agent-shell-dashboard-active-sessions-file nil)
              (agent-shell-dashboard-summary-archive-file nil)
              (agent-shell-dashboard-claude-projects-dir jsonl-root)
              (agent-shell-dashboard-forgotten-ids-file forgotten-tmp))
          (asd-test--make-jsonl-fixture jsonl-root "a" "keep-me")
          (asd-test--make-jsonl-fixture jsonl-root "b" "forget-me")
          (agent-shell-dashboard--tombstone-session "forget-me")
          (let* ((rows (agent-shell-dashboard--rows))
                 (ids  (--map (plist-get it :session-id) rows)))
            (should (member "keep-me" ids))
            (should-not (member "forget-me" ids))))
      (delete-file forgotten-tmp)
      (delete-directory jsonl-root t))))

(ert-deftest asd-test-forget-tombstones-the-session ()
  "Calling `--forget-session' writes to the tombstone file."
  (let ((active-tmp    (make-temp-file "asd-active-"    nil ".el"))
        (archive-tmp   (make-temp-file "asd-summary-"   nil ".el"))
        (forgotten-tmp (make-temp-file "asd-forgotten-" nil ".el")))
    (unwind-protect
        (let ((agent-shell-dashboard-active-sessions-file  active-tmp)
              (agent-shell-dashboard-summary-archive-file  archive-tmp)
              (agent-shell-dashboard-forgotten-ids-file    forgotten-tmp))
          (agent-shell-dashboard--forget-session "doomed")
          (should (member "doomed"
                          (agent-shell-dashboard--read-forgotten-ids))))
      (delete-file active-tmp)
      (delete-file archive-tmp)
      (delete-file forgotten-tmp))))


;;;; --- Search ----------------------------------------------------

(ert-deftest asd-test-search-jumps-to-match ()
  "Search labels rows by project | summary | preview and jumps on RET."
  (let ((agent-shell-dashboard-active-sessions-file nil)
        (agent-shell-dashboard-summary-archive-file nil)
        (agent-shell-dashboard-claude-projects-dir nil)
        (agent-shell-dashboard-pinned-projects nil)
        (buf (generate-new-buffer "*asd-search-test*")))
    (unwind-protect
        (with-current-buffer buf
          (agent-shell-dashboard-mode)
          ;; Hand-render two rows so we don't need real agent-shell state.
          (let ((inhibit-read-only t))
            (insert (propertize "alpha | tag-one | hello\n"
                                'dg-row '(:session-id "s1" :cwd "/x/alpha"
                                          :summary "tag-one" :status closed
                                          :last-prompt-text "hello")))
            (insert (propertize "beta  | tag-two | world\n"
                                'dg-row '(:session-id "s2" :cwd "/x/beta"
                                          :summary "tag-two" :status closed
                                          :last-prompt-text "world"))))
          (setq agent-shell-dashboard--row-positions '(1 25))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _)
                       ;; pick the candidate containing "tag-two"
                       (--find (s-contains? "tag-two" it) collection))))
            (goto-char (point-min))
            (agent-shell-dashboard-search)
            (should (= (point) 25))))
      (kill-buffer buf))))

(provide 'agent-shell-dashboard-test)
;;; agent-shell-dashboard-test.el ends here
