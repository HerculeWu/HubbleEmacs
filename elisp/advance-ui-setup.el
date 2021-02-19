
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'bitmap))

(use-package info-colors 
  :ensure t 
  :hook ('Info-selection-hook . 'info-colors-fontify-node))

(use-package nyan-mode
  :ensure t
  :hook (after-init . nyan-mode)
  :config
  (setq nyan-wavy-trail t
		nyan-animate-nyancat t))
(message "advance ui inited!")
(provide 'advance-ui-setup)
