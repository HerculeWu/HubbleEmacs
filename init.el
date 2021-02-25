(setq custom-file "~/.emacs.d/auto-custom-vars.el")

(defvar dumped-load-path nil
  "Not nil when using dump.")

(when dumped-load-path
  (setq load-path dumped-load-path)
  (setq warning-minimum-level :emergency)
  (global-font-lock-mode t)
  (transient-mark-mode t)
  (load "~/.emacs.d/init-after.el")
  )

(unless dumped-load-path
  (load "~/.emacs.d/init-befor.el")
  (load "~/.emacs.d/init-after.el"))

(when (file-exists-p custom-file)
    (load-file custom-file))
