

(use-package treemacs-evil
  :config
  (defun herl-close-tree ()
    "close tree view"
    (interactive)
    (treemacs-select-window)
    (treemacs-quit)
    )
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-m"
   "ft" '(:ignore t :which-key "file tree")
   "ftt" '(treemacs :which-key "open file tree")
   "fts" '(treemacs-select-window :which-key "switch to tree")
   "ftq" '(herl-close-tree :which-key "quit tree")
   )
  )
(provide 'treemacs-setup)
