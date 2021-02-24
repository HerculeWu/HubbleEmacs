;; init for package management


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa"))

(package-initialize)
(unless package-archive-contents
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
   (package-refresh-contents)
   (package-install 'use-package))
(eval-and-compile
    (setq use-package-always-ensure t) 
    (setq use-package-always-defer nil)
    (setq use-package-always-demand t)
    (setq use-package-expand-minimally t)
    (setq use-package-verbose t))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(package-install 'auctex)

(provide 'package-init)
