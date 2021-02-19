
(load "~/.emacs.d/elisp/early-init.el")

(setq dump-exclude-packages '(helm-core
			      auctex))

(dolist (package package-activated-list)
  (unless (member package dump-exclude-packages)
    (require package)))


(setq dumped-load-path load-path)

(dump-emacs-portable "~/.emacs.d/emacs.pdmp")

