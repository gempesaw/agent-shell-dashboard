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
;; Code is split across these modules:
;;   - agent-shell-dashboard-data       row sources, persistence, helpers
;;   - agent-shell-dashboard-render     cells, columns, refresh, jump
;;   - agent-shell-dashboard-commands   mode, keymap, interactive commands
;;   - agent-shell-dashboard-tracking   opt-in advice, summary capture
;;   - agent-shell-dashboard-transient  ? menu
;;
;; The dashboard does not track per-session enrichment data unless
;; `agent-shell-dashboard-setup' is called.  When unset, the dashboard
;; degrades gracefully: no summary, no awaiting-state, no preview line.

;;; Code:

(require 'agent-shell-dashboard-commands)
(require 'agent-shell-dashboard-tracking)
(require 'agent-shell-dashboard-transient)

(provide 'agent-shell-dashboard)
;;; agent-shell-dashboard.el ends here
