;; early init for dump

(add-to-list 'load-path "~/.emacs.d/elisp/")
(load "~/.emacs.d/config.el")
;;(require 'basic-ui-setup)
(require 'package-init)

(setq setup-list '(
		   theme
		   smartparens-setup
		   evil-setup
		   helm-setup
		   which-key-setup
		   general-setup
		   winum-setup
		   advance-ui-setup
		   yasnippet-setup
		   treemacs-setup
		   latex-setup
		   org-setup
		   command-log-setup
		   path-setup
		   lsp-setup
		   magit-setup
		   python-setup
		   ))
(dolist (setup setup-list)
  (require setup))


(provide 'early-init)
