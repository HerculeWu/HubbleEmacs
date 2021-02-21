;; setup company mode


(use-package company
:config
(setq company-idle-delay 0)
(setq company-selection-wrap-around t)
(setq company-minimum-prefix-length 2)
(company-tng-configure-default)
(global-company-mode)
:bind (
       (:map company-active-map
	     ("RET" . company-complete-selection)
	     ("C-n" . company-select-next)
	     ("C-e" . company-select-previous)
	     )
       )
)
(use-package company-box
  :hook (company-mode . company-box-mode))

(provide 'company-setup)
