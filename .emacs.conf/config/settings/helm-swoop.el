(require 'helm-swoop)

(global-set-key (kbd "C-c h s s") 'helm-swoop)
(global-set-key (kbd "C-c h s b") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c h s m") 'helm-multi-swoop)
(global-set-key (kbd "C-c h s a") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows t)
;; If you prefer fuzzy matching
(setq helm-swoop-use-fuzzy-match t)
