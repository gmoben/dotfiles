(add-to-list 'auto-mode-alist '("\\.py_\\.?\\(_?EXAMPLE|PRE_VERSION\\)$" . python-mode))

(when (executable-find "pyls")
  (add-hook 'python-mode-hook #'company-mode)
  (add-hook 'python-mode-hook #'lsp))
