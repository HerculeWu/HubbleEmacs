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
		   ))
(dolist (setup setup-list)
  (require setup))

;;(require 'theme)
;;(require 'smartparens-setup)
;;(require 'evil-setup)
;;(require 'helm-setup)
;;(require 'which-key-setup)
;;(require 'general-setup)
;;(require 'winum-setup)
;;(require 'advance-ui-setup)
;;(require 'yasnippet-setup)
;;(require 'treemacs-setup)
;;(require 'latex-setup)
;;(require 'org-setup)
;;(require 'command-log-setup)
;;(require 'path-setup)
;;(require 'lsp-setup)

(provide 'early-init)
