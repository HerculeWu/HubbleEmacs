


(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(defun latex/change-env ()
  (interactive)
  (LaTeX-environment t))

(defun latex/font-bold () (interactive) (TeX-font nil ?\C-b))
(defun latex/font-code () (interactive) (TeX-font nil ?\C-t))
(defun latex/font-emphasis () (interactive) (TeX-font nil ?\C-e))
(defun latex/font-italic () (interactive) (TeX-font nil ?\C-i))
(defun latex/font-clear () (interactive) (TeX-font nil ?\C-d))
(defun latex/font-oblique () (interactive) (TeX-font nil ?\C-s))

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
		"e" '(:ignore t :wk "environment")
		"e i" '(LaTeX-environment :wk "insert")
		"e c" '(latex/change-env :wk "change")
		"f" '(:ignore t :wk "insert font")
		"fb"  '(latex/font-bold :wk "bold")
		"fc"  '(latex/font-code :wk "code")
		"fe"  '(latex/font-emphasis :wk "emphasis")
		"fi"  '(latex/font-italic :wk "italic")
		"fr"  '(latex/font-clear :wk "clear")
		"fo"  '(latex/font-oblique :wk "oblique")
		)
	    )
	  )
 )
(provide 'latex-setup)
