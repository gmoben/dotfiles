(use-package ewal
  :init
  (setq ewal-dark-palette-p t)
  (setq ewal-force-tty-colors-in-daemon-p nil)
  (setq ewal-use-built-in-on-failure-p nil))

(use-package ewal-doom-themes
  :config
  (load-theme 'ewal-doom-one t)
  (enable-theme 'ewal-doom-one))

(use-package all-the-icons)
