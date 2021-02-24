

(defvar dumped-load-path nil
  "Not nil when using dump.")

(when dumped-load-path
  (setq load-path dumped-load-path)
  (setq warning-minimum-level :emergency)
  (global-font-lock-mode t)
  (transient-mark-mode t)
  (enable-theme 'doom-one)
  (require 'after-init)
  )


(unless dumped-load-path
  (add-to-list 'load-path "~/.emacs.d/elisp/")
  (require 'early-init)
  (require 'after-init)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(magit evil-collection lsp-treemacs helm-lsp lsp-ui markdown-mode lsp-mode python-mode exec-path-from-shell command-log-mode company-box org-bullets evil-org auctex treemacs-evil treemacs yasnippet winum which-key use-package smartparens quelpa nyan-mode info-colors highlight-indent-guides helm general evil doom-themes doom-modeline dashboard company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
