;; (use-package ewal
;;   :ensure t
;;   :config (progn
;;             (setq ewal-use-built-in-on-failure-p t)
;;             (setq ewal-built-in-palette "sexy-material")
;;             (ewal-load-colors)
;;             )
;;   ))
(use-package doom-themes
  :ensure t
  :config (progn
            (load-theme 'doom-manegarm t)
            (enable-theme 'doom-manegarm)))

(use-package all-the-icons
  :ensure t)
