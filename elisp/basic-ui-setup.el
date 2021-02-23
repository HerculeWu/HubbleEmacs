;; basic ui setups
(tool-bar-mode -1)

(scroll-bar-mode -1)

(setq inhibit-splash-screen 1)

(set-frame-font "Monaco 18" nil t)

(show-paren-mode)

(setq make-backup-files nil)

(setq initial-frame-alist (quote ((fullscreen . maximized))))

(global-hl-line-mode)

(provide 'basic-ui-setup)
