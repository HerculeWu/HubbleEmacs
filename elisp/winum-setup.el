;; winum

(use-package winum
  :config
  (winum-mode)
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-m"
   "1" '(winum-select-window-1 :which-key "window 1")
   "2" '(winum-select-window-2 :which-key "window 2")
   "3" '(winum-select-window-3 :which-key "window 3")
   "4" '(winum-select-window-4 :which-key "window 4")
   "5" '(winum-select-window-5 :which-key "window 5")
   "6" '(winum-select-window-6 :which-key "window 6")

   )
  )

(message "winum inided!")
(provide 'winum-setup)
