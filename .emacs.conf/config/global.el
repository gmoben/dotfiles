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

(set-face-attribute 'default (selected-frame) :height 135)

;; Global modes
(delete-selection-mode 1)
(semantic-mode 1)

;; easier movement
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; tabs
;; https://stackoverflow.com/questions/5528349/emacs-makefile-tab-size#5528393
; Turn on tabs
(setq indent-tabs-mode t)
(setq-default indent-tabs-mode t)

;; Bind the TAB key
(global-set-key (kbd "TAB") 'self-insert-command)

;; Set the tab width
(setq-default tab-width 4)
(setq tab-width 4)
(setq c-basic-indent 4)
(setq c-basic-offset 4)

(defun file-if-exists (filename) "return file if it exists, else nil" nil
       (if (file-exists-p filename)
           filename
         nil))

;; https://www.emacswiki.org/emacs/SortWords
(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "[\\w\\-_\\+]+" "\\&" beg end))

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))


;; Duplicate line
(global-set-key "\C-x\C-d" "\C-a\C-k\C-k\C-y\C-y\C-b\C-a")

;; Follow symlinks without asking
(setq vc-follow-symlinks nil)

;; visual-line-mode for line wrapping
(setq global-visual-line-mode 1)

(global-set-key (kbd "C-c TAB") 'indent-region)
(global-set-key (kbd "C-x TAB") 'indent-rigidly)

(global-set-key (kbd "C-c e b") 'eval-buffer)
(global-set-key (kbd "C-c e r") 'eval-region)

(set-language-environment 'utf-8)

;; Use shift-arrows instead of 'C-x o' to switch windows
;; (windmove-default-keybindings)
