;; projectile
(require 'projectile)
(projectile-mode 1)
(helm-projectile-on)
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-enable-caching t)
(add-hook 'emacs-startup-hook 'projectile-mode)
