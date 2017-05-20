(require 'helm)

;; General options
(setq helm-M-x-fuzzy-match t)
(helm-fuzzier-mode)
(helm-descbinds-mode)

;; Remapped bindings
(global-set-key [remap execute-extended-command] 'helm-M-x)
(global-set-key [remap find-file] 'helm-find-files)
(global-set-key [remap list-buffers] 'helm-buffers-list)
(global-set-key [remap yank-pop] 'helm-show-kill-ring)
(global-set-key [remap describe-mode] 'helm-describe-modes)
