


(use-package page-break-lines
  :ensure t
  :hook (after-init . page-break-lines-mode))



(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "welcom back!")
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t) 
  (setq dashboard-set-navigator t)
  )

(message "dashboard inited!")
(provide 'dashboard-setup)
