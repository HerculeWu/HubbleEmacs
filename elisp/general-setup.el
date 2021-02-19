;; general

(use-package general :ensure t
  :config
  (general-override-mode)
  (general-evil-setup)
  (general-create-definer global-leader
    :states '(normal insert emacs visual)
    :prefix normal-global-leader-key
    :non-normal-prefix non-normal-leader-key)
  (general-create-definer major-leader
    :states '(normal insert emacs visual)
    :prefix normal-major-mode-leader-key
    :non-normal-prefix non-normal-major-mode-leader-key)
  (general-nmap "SPC m" (general-simulate-key "," :which-key "major mode"))
  (global-leader 
   "f" '(:ignore t :which-key "file")
   "ff" '(helm-find-files :which-key "open file")
   "fs" '(save-buffer :which-key "save")
   "fd" '(:ignore t :which-key "dot files")
   "fde" '(open-init-file :which-key "open init.el")
   "w" '(:ignore t :which-key "window")
   "ww" '(other-window :which-key "other window")
   "wh" '(evil-window-left :which-key "window below")
   "wn" '(evil-window-down :which-key "window down")
   "we" '(evil-window-up :which-key "window up")
   "wi" '(evil-window-right :which-key "window right")
   "w/" '(split-window-right :which-key "split right")
   "w-" '(split-window-below :which-key "split below")
   "wd" '(delete-window :which-key "delete window")
   "wm" '(delete-other-windows :which-key "maxmize")
   "b" '(:ignore t :which-key "buffer")
   "bb" '(helm-mini :which-key "buffer list")
   "bn" '(next-buffer :which-key "next buffer")
   "be" '(previous-buffer :which-key "previous buffer")
   "q" '(save-buffers-kill-terminal :which-key "quit")
   "SPC" '(helm-M-x :which-key "M-x")
   )
  )


(message "general inided!")
(provide 'general-setup)
