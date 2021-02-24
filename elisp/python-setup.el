(use-package python-mode
  :hook (python-mode . lsp)
  :custom
  (python-shell-interpreter "python3"))
(use-package py-isort)

(provide 'python-setup)
