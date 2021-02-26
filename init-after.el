(tool-bar-mode -1)

(scroll-bar-mode -1)

(setq inhibit-splash-screen 1)

(set-frame-font "Monaco 18" nil t)

(show-paren-mode)

(setq make-backup-files nil)

(global-hl-line-mode)

(enable-theme 'doom-one)

(use-package company
:config
(setq company-idle-delay 0)
(setq company-selection-wrap-around t)
(setq company-minimum-prefix-length 3)
(company-tng-configure-default)
(global-company-mode)
:bind (
       (:map company-active-map
	     ("RET" . company-complete-selection)
	     ("C-n" . company-select-next)
	     ("C-e" . company-select-previous)
	     )
       )
)
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package all-the-icons)

(use-package doom-modeline
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-lsp t)
  )

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

(use-package vterm
  :config
  (setq vterm-max-scrollback 10000))

(setq gc-cons-threshold 100000000)

(setq read-process-output-max (* 1024 1024))

(setq garbage-collection-messages t)

(defvar k-gc-timer
  (run-with-idle-timer 15 t
		       'garbage-collect))

(setq xwidget-webkit-enable-plugins t)
