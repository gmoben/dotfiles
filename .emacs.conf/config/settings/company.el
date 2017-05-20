(require 'company)
(require 'helm-company)

;; Always global
(add-hook 'after-init-hook 'global-company-mode)

;; Backends
(add-to-list 'company-backends 'company-jedi)
(add-to-list 'company-backends 'company-go)

;; Key bindings
(define-key company-mode-map (kbd "C-:") 'helm-company)
(define-key company-active-map (kbd "C-:") 'helm-company)
(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
