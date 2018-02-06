(require 'rust-mode)

(setq rust-format-on-save t)

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
