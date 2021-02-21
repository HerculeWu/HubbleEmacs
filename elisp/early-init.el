;; early init for dump

(add-to-list 'load-path "~/.emacs.d/elisp/")
(load "~/.emacs.d/config.el")
;;(require 'basic-ui-setup)
(require 'package-init)
(require 'theme)
;;(require 'company-setup)
(require 'smartparens-setup)
(require 'evil-setup)
(require 'helm-setup)
(require 'which-key-setup)
(require 'general-setup)
(require 'winum-setup)
;;(require 'dashboard-setup)
(require 'advance-ui-setup)
;;(require 'mode-line-setup)
(require 'yasnippet-setup)
(require 'treemacs-setup)
(require 'latex-setup)
(require 'org-setup)
(require 'command-log-setup)

(provide 'early-init)
