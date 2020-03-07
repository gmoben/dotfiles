;; Powerline
(use-package powerline :ensure t)
(use-package spaceline-config
  :ensure t
  :after (powerline)
  :init (
		 (load-theme 'spaceline-emacs-theme)))
