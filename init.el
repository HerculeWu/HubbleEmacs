

(defvar dumped-load-path nil
  "Not nil when using dump.")

(when dumped-load-path
  ;;恢复load-path
  (setq load-path dumped-load-path)
  ;; 修改一下报错等级，这个读者按心意加，不影响dump
  (setq warning-minimum-level :emergency)
  ;; 一些功能失常的mode，需要重新开启
  (global-font-lock-mode t)
  (transient-mark-mode t)
  (enable-theme 'doom-one)
  (require 'after-init)
  
  
  )


(unless dumped-load-path
  (add-to-list 'load-path "~/.emacs.d/elisp/")


  (require 'early-init)
  (require 'after-init)
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(package-selected-packages
     '(yasnippet quelpa nyan-mode info-colors highlight-indent-guides dashboard winum general which-key helm evil smartparens company doom-modeline doom-themes use-package)))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )
  )




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auctex treemacs-evil treemacs yasnippet winum which-key use-package smartparens quelpa nyan-mode info-colors highlight-indent-guides helm general evil doom-themes doom-modeline dashboard company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
