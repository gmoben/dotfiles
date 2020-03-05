(use-package ewal
  :ensure t
  :config (progn
            (setq ewal-dark-palette-p t)
            (setq ewal-force-tty-colors-in-daemon-p t)
            (setq ewal-use-built-in-on-failure-p nil)))

(use-package ewal-spacemacs-themes
  :ensure t
  :config (progn
            (load-theme 'ewal-spacemacs-classic t)
            (enable-theme 'ewal-spacemacs-classic)))

(use-package all-the-icons
  :ensure t)
