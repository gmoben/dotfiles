;;; global.el --- Global Emacs settings -*- lexical-binding: t; -*-

;; Declare external variables to suppress byte-compiler warnings
(defvar make-auto-save-file-name-function)
(defvar c-basic-indent)
(defvar c-basic-offset)
(defvar mouse-wheel-scroll-amount)
(defvar mouse-wheel-progressive-speed)
(defvar auto-revert-verbose)

;; Store auto-save files in ~/.emacs.d with hash-based names to avoid long filenames
;; This prevents "File name too long" errors for deeply nested project paths
(defun make-auto-save-file-name-hash ()
  "Create an auto-save file name using SHA1 hash to keep names short."
  (let* ((filename (buffer-file-name))
         (basename (if filename
                      (file-name-nondirectory filename)
                      (buffer-name)))
         (hash (if filename
                  (sha1 filename)
                  (sha1 (buffer-name))))
         (auto-save-dir (expand-file-name "auto-save/" user-emacs-directory)))
    (unless (file-directory-p auto-save-dir)
      (make-directory auto-save-dir t))
    (expand-file-name (format "#%s#%s#" hash basename) auto-save-dir)))

(setq make-auto-save-file-name-function 'make-auto-save-file-name-hash)

(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups/" user-emacs-directory))))

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

;; Faster scrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed t)
(setq scroll-step 3)
(setq scroll-conservatively 10)

;; Auto-revert buffers without prompting when files change externally
;; This prevents "file has changed, reload?" prompts when Claude edits files
(global-auto-revert-mode 1)
(setq revert-without-query '(".*"))  ; Auto-revert all files without asking
(setq auto-revert-verbose nil)       ; Don't show messages when reverting

(global-set-key (kbd "C-c TAB") 'indent-region)
(global-set-key (kbd "C-x TAB") 'indent-rigidly)

(global-set-key (kbd "C-c e b") 'eval-buffer)
(global-set-key (kbd "C-c e r") 'eval-region)

(set-language-environment 'utf-8)

;; Use shift-arrows instead of 'C-x o' to switch windows
;; (windmove-default-keybindings)
