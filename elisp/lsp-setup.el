(use-package lsp-mode)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(setq lsp-completion-provider :capf)
(setq lsp-idle-delay 0.500)
(setq lsp-auto-guess-root nil)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-position 'at-point)

(provide 'lsp-setup)

