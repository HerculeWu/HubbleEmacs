;; basic ui setups
(tool-bar-mode -1)

(scroll-bar-mode -1)

(setq inhibit-splash-screen 1)

(set-frame-font "Monaco 18" nil t)

(show-paren-mode)

(setq make-backup-files nil)

(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(provide 'basic-ui-setup)