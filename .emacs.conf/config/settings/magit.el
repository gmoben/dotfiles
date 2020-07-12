;; magit-gh-pulls
;;(require 'magit-gh-pulls)
;;(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

(use-package exec-path-from-shell)

(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

(use-package magit
  :ensure t
  :commands (magit-status)
  :bind ("C-c C-g" . magit-status))
