(use-package magit)
(global-leader
  "g" '(:ignore t :which-key "git")
  "gg" '(magit-status :which-key "status")
  )
