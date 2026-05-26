;;; agent-shell-dashboard-transient.el --- Transient dispatch -*- lexical-binding: t; -*-

;;; Commentary:

;; The `?' help / dispatch menu, bound onto the dashboard keymap
;; once both transient and the commands module are available.

;;; Code:

(require 'transient)
(require 'agent-shell-dashboard-commands)

(transient-define-prefix agent-shell-dashboard-transient ()
  "Help / dispatch menu for the agent-shell dashboard."
  ["Agent Shell Dashboard"
   ["Navigate"
    ("n" "next row"      agent-shell-dashboard-next)
    ("p" "previous row"  agent-shell-dashboard-previous)
    ("j" "jump column"   agent-shell-dashboard-jump)
    ("g" "refresh"       agent-shell-dashboard-refresh)]
   ["Row"
    ("RET" "visit"               agent-shell-dashboard-visit)
    ("o"   "open in other window" agent-shell-dashboard-visit-other-window)
    ("s"   "send (viewport)"     agent-shell-dashboard-send)
    ("f"   "fork"                agent-shell-dashboard-fork)
    ("r"   "reload"              agent-shell-dashboard-reload)
    ("k"   "kill / forget"       agent-shell-dashboard-kill)
    ("m"   "mark as read"        agent-shell-dashboard-mark-as-read)]
   ["Sessions"
    ("N" "new session"               agent-shell-dashboard-start-new-session)
    ("M" "new session (pick repo)"   agent-shell-dashboard-start-new-session-pick-repo)]
   ["Dashboard"
    ("q" "quit and restore layout" agent-shell-dashboard-quit)]])

(define-key agent-shell-dashboard-mode-map (kbd "?") #'agent-shell-dashboard-transient)

(provide 'agent-shell-dashboard-transient)
;;; agent-shell-dashboard-transient.el ends here
