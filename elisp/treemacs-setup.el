

(use-package treemacs-evil
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-m"
   "ft" '(:ignore t :which-key "file tree")
   "ftt" '(treemacs :which-key "open file tree")
   "fts" '(treemacs-select-window :which-key "switch to tree")
   )
  )
(provide 'treemacs-setup)
