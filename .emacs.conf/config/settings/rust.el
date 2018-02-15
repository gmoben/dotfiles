(require 'rust-mode)
(require 'cargo)
(require 'racer)

(setq rust-format-on-save t)

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(add-hook 'rust-mode-hook 'cargo-minor-mode)

(setq racer-cmd "/home/ben/.cargo/bin/racer")
(setq racer-rust-src-path "/code/ext/rust/src")
(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'rust-mode-hook 'company-mode)
