


(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (progn
	      (major-leader 'LaTeX-mode-map
		"p" '(:ignore t :wk "preview")
		"p e" '(preview-environment :wk "environment")
		"p l" '(preview-region :wk "region")
		"p s" '(preview-section :wk "section")
		"p p" '(preview-at-point :wk "at point")
		"p c" '(:ignore t :wk "clear preview")
		"p c b" '(preview-clearout-buffer :wk "buffer")
		"p c s" '(preview-clearout-section :wk "section")
		"p c p" '(preview-clearout-at-point :wk "at point")
		"e" '(LaTeX-environment :wk "insert environment")
		)
	    )
	  )
 )
(provide 'latex-setup)
