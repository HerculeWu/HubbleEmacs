(use-package magit)
(global-leader
  "g" '(:ignore t :which-key "git")
  "gg" '(magit-status :which-key "status")
  "gs" '(magit-stage-file :which-key "stage")
  "gu" '(magit-unstage-file :which-key "unstage")
  )
(dolist (state '(normal motion))
  (evil-define-key state with-editor-mode-map
    (concat normal-major-mode-leader-key normal-major-mode-leader-key) 'with-editor-finish
    (concat normal-major-mode-leader-key "c") 'with-editor-finish
    (concat normal-major-mode-leader-key "k") 'with-editor-cancel
    )
  )
(provide 'magit-setup)
