

(use-package all-the-icons)

(use-package doom-modeline
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-lsp t)
  )

(message "mode line inited!")
(provide 'mode-line-setup)
