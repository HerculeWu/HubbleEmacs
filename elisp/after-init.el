
(require 'basic-ui-setup)
(require 'company-setup)
(require 'dashboard-setup)
(require 'mode-line-setup)
;;(require 'path-setup)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq garbage-collection-messages t)
(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       'garbage-collect))
(setq xwidget-webkit-enable-plugins t)
(provide 'after-init)
