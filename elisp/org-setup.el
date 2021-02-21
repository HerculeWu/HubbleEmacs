(use-package org
  :config
  (setq org-ellipsis " ▼")
  (setq org-hide-emphasis-markers t)
  )
(use-package evil-org
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
(use-package org-bullets
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
(add-hook 'org-mode-hook
	  (lambda ()
	    (progn
	      (evil-org-mode)
	      (org-bullets-mode)
	      (major-leader 'org-mode-map
		"," 'org-ctrl-c-ctrl-c
		"*" 'org-ctrl-c-star
		"*"	'org-ctrl-c-star
		"RET"	'org-ctrl-c-ret
		"-"	'org-ctrl-c-minus
		"'"	'org-edit-special
		"^"	'org-sort
		"/"	'org-sparse-tree
		"."	'org-time-stamp
		"!"	'org-time-stamp-inactive
		"a"	'org-agenda
		"b"	'org-tree-to-indirect-buffer
		"A"	'org-archive-subtree
		"c"	'org-capture
		"C"	'evil-org-recompute-clocks
		"d"	'org-deadline
		"D"	'org-insert-drawer
		"e"	'org-export-dispatch
		"f"	'org-set-effort
		"K"	'org-clock-in
		"l"	'org-open-at-point
		"O"	'org-clock-out
		"P"	'org-set-property
		"q"	'org-clock-cancel
		"R"	'org-refile
		"s"	'org-schedule
		"T"	'org-show-todo-tree
		"I"	'org-shiftright
		"H"	'org-shiftleft
		"E"	'org-shiftup
		"N"	'org-shiftdown
		)
	      )))


(provide 'org-setup)
