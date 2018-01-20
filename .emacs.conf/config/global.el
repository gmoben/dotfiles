;; Store auto-save files in system temp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; save hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Save before closing
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

;; smart open line
;; http://emacsredux.com/blog/2013/06/15/open-line-above/
(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "C-x <down>") 'smart-open-line)
(global-set-key (kbd "C-x <up>") 'smart-open-line-above)

;; Scale all buffers
(define-globalized-minor-mode
  global-text-scale-mode
  text-scale-mode
  (lambda () (text-scale-mode 1)))

(defun global-text-scale-adjust (inc)
  (interactive)
  (text-scale-set 1)
  (kill-local-variable 'text-scale-mode-amount)
  (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
  (global-text-scale-mode 1))

(set-face-attribute 'default (selected-frame) :height 135)

(global-set-key (kbd "M-0")
		'(lambda () (interactive)
		   (global-text-scale-adjust (- text-scale-mode-amount))
		   (global-text-scale-mode -1)))
(global-set-key (kbd "M-+")
		'(lambda () (interactive) (global-text-scale-adjust 1)))
(global-set-key (kbd "M--")
		'(lambda () (interactive) (global-text-scale-adjust -1)))

;; Globally enabled modes
(delete-selection-mode 1)
(xterm-mouse-mode 1)
(semantic-mode 1)

;; easier movement
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

(defun scroll-up-some-lines ()
  "Move page up 3 logical lines"
  (interactive)
  (scroll-down 3)
  )

(defun scroll-down-some-lines ()
  "Move page down 10 logical lines"
  (interactive)
  (scroll-up 3))

(when (string-equal system-type "gnu/linux")
  (progn
    (global-set-key (kbd "<mouse-4>") 'scroll-up-some-lines) ; wheel up
    (global-set-key (kbd "<mouse-5>") 'scroll-down-some-lines) ; wheel down
    )
  )
