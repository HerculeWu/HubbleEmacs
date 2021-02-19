;; helm setup

(use-package helm
  :config
  (require 'helm-config)
  (helm-autoresize-mode 1)
  (setq helm-autoresize-max-height 50)
  (setq helm-autoresize-min-height 15)
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x C-b" . helm-mini)
  ("M-y" . helm-show-kill-ring)
  )

(message "helm inited!")
(provide 'helm-setup)
