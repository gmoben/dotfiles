;; projectile
(require 'projectile)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(helm-projectile-on)
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'projectile-find-dir) ;; Ask for subdir on project select then open in dired
(setq projectile-find-dir-includes-top-level t) ;; Include top level when asking for subdir
;; (setq projectile-switch-project-action 'projectile-dired) ;; Open dired on project select
(setq projectile-enable-caching t)
(add-hook 'emacs-startup-hook 'projectile-mode)
