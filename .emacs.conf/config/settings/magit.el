;; magit-gh-pulls
;;(require 'magit-gh-pulls)
;;(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

(global-set-key (kbd "C-c C-g") 'magit-status)

(require 'exec-path-from-shell)
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
