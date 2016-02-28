
(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			                          ("melpa" . "https://melpa.org/packages/")))

;; other packages
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; handle tmux's xterm-keys
;; put the following line in your ~/.tmux.conf:
;;   setw -g xterm-keys on
(if (getenv "TMUX")
    (progn
      (let ((x 2) (tkey ""))
	(while (<= x 8)
	  ;; shift
	  (if (= x 2)
	      (setq tkey "S-"))
	  ;; alt
	  (if (= x 3)
	      (setq tkey "M-"))
	  ;; alt + shift
	  (if (= x 4)
	      (setq tkey "M-S-"))
	  ;; ctrl
	  (if (= x 5)
	      (setq tkey "C-"))
	  ;; ctrl + shift
	  (if (= x 6)
	      (setq tkey "C-S-"))
	  ;; ctrl + alt
	  (if (= x 7)
	      (setq tkey "C-M-"))
	  ;; ctrl + alt + shift
	  (if (= x 8)
	      (setq tkey "C-M-S-"))

	  ;; arrows
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d A" x)) (kbd (format "%s<up>" tkey)))
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d B" x)) (kbd (format "%s<down>" tkey)))
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d C" x)) (kbd (format "%s<right>" tkey)))
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d D" x)) (kbd (format "%s<left>" tkey)))
	  ;; home
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d H" x)) (kbd (format "%s<home>" tkey)))
	  ;; end
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d F" x)) (kbd (format "%s<end>" tkey)))
	  ;; page up
	  (define-key key-translation-map (kbd (format "M-[ 5 ; %d ~" x)) (kbd (format "%s<prior>" tkey)))
	  ;; page down
	  (define-key key-translation-map (kbd (format "M-[ 6 ; %d ~" x)) (kbd (format "%s<next>" tkey)))
	  ;; insert
	  (define-key key-translation-map (kbd (format "M-[ 2 ; %d ~" x)) (kbd (format "%s<delete>" tkey)))
	  ;; delete
	  (define-key key-translation-map (kbd (format "M-[ 3 ; %d ~" x)) (kbd (format "%s<delete>" tkey)))
	  ;; f1
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d P" x)) (kbd (format "%s<f1>" tkey)))
	  ;; f2
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d Q" x)) (kbd (format "%s<f2>" tkey)))
	  ;; f3
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d R" x)) (kbd (format "%s<f3>" tkey)))
	  ;; f4
	  (define-key key-translation-map (kbd (format "M-[ 1 ; %d S" x)) (kbd (format "%s<f4>" tkey)))
	  ;; f5
	  (define-key key-translation-map (kbd (format "M-[ 15 ; %d ~" x)) (kbd (format "%s<f5>" tkey)))
	  ;; f6
	  (define-key key-translation-map (kbd (format "M-[ 17 ; %d ~" x)) (kbd (format "%s<f6>" tkey)))
	  ;; f7
	  (define-key key-translation-map (kbd (format "M-[ 18 ; %d ~" x)) (kbd (format "%s<f7>" tkey)))
	  ;; f8
	  (define-key key-translation-map (kbd (format "M-[ 19 ; %d ~" x)) (kbd (format "%s<f8>" tkey)))
	  ;; f9
	  (define-key key-translation-map (kbd (format "M-[ 20 ; %d ~" x)) (kbd (format "%s<f9>" tkey)))
	  ;; f10
	  (define-key key-translation-map (kbd (format "M-[ 21 ; %d ~" x)) (kbd (format "%s<f10>" tkey)))
	  ;; f11
	  (define-key key-translation-map (kbd (format "M-[ 23 ; %d ~" x)) (kbd (format "%s<f11>" tkey)))
	  ;; f12
	  (define-key key-translation-map (kbd (format "M-[ 24 ; %d ~" x)) (kbd (format "%s<f12>" tkey)))
	  ;; f13
	  (define-key key-translation-map (kbd (format "M-[ 25 ; %d ~" x)) (kbd (format "%s<f13>" tkey)))
	  ;; f14
	  (define-key key-translation-map (kbd (format "M-[ 26 ; %d ~" x)) (kbd (format "%s<f14>" tkey)))
	  ;; f15
	  (define-key key-translation-map (kbd (format "M-[ 28 ; %d ~" x)) (kbd (format "%s<f15>" tkey)))
	  ;; f16
	  (define-key key-translation-map (kbd (format "M-[ 29 ; %d ~" x)) (kbd (format "%s<f16>" tkey)))
	  ;; f17
	  (define-key key-translation-map (kbd (format "M-[ 31 ; %d ~" x)) (kbd (format "%s<f17>" tkey)))
	  ;; f18
	  (define-key key-translation-map (kbd (format "M-[ 32 ; %d ~" x)) (kbd (format "%s<f18>" tkey)))
	  ;; f19
	  (define-key key-translation-map (kbd (format "M-[ 33 ; %d ~" x)) (kbd (format "%s<f19>" tkey)))
	  ;; f20
	  (define-key key-translation-map (kbd (format "M-[ 34 ; %d ~" x)) (kbd (format "%s<f20>" tkey)))

	  (setq x (+ x 1))
	  ))
      )
    )

;; iTerm remappings for RET
(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "<f12>") 'org-table-copy-down)
     (define-key org-mode-map (kbd "<M-f12>") 'org-meta-return)
     (define-key org-mode-map (kbd "<C-f12>") 'org-insert-heading-respect-content)
     (define-key org-mode-map (kbd "<C-S-f12>") 'org-insert-todo-heading-respect-content)
     (define-key org-mode-map (kbd "<M-S-f12>") 'org-insert-todo-heading)
  ))

;; Shortcuts
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c g") 'magit-status)

;; org-mode preferences
(setq org-log-done 'time)
(setq org-log-done 'note)
(setq org-refile-targets (quote ((org-agenda-files :tag . ""))))
;; Save clock history across sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
;; capture setup
(setq org-default-notes-file "~/notes/refile.org")
(define-key global-map "\C-cc" 'org-capture)

;; projectile
(projectile-global-mode)

;; PATH
(if (not (getenv "TERM_PROGRAM"))
    (setenv "PATH"
	(shell-command-to-string "source $HOME/.bashrc && printf $PATH")))
(setq exec-path (split-string (getenv "PATH") ":"))
(setq path-to-ctags "/user/bwarren/.local/bin/ctags") ;; defaults to XCode ctags, so change it!

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b61c55259c639a54628f91452b060b99c550a1269eb947e372321b806b68f114" "ac2b1fed9c0f0190045359327e963ddad250e131fbf332e80d371b2e1dbc1dc4" "519d1b3cb7345cc9be10b4b0489436ae2d1b0690470d8d78f8e4e1ff19b83a86" default)))
 '(encourage-mode nil)
 '(exec-path
   (quote
    ("/Users/bwarren/.gem/ruby/2.0.0/bin" "/Users/bwarren/Library/Python/2.7/lib/python/bin" "/Users/bwarren/Library/Python/2.7/bin/" "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Users/bwarren/.gem/ruby/2.0.0/bin" "/Users/bwarren/.local/bin" "/Users/bwarren/bin")))
 '(global-flycheck-mode t)
 '(org-agenda-files (quote ("~/notes/work.org" "~/notes/home.org")))
 '(org-capture-templates
   (quote
    (("w" "Work TODO items" entry
      (file+headline "~/notes/work.org" "Refile")
      "* TODO ")
     ("h" "Home TODO items" entry
      (file+headline "~/notes/home.org" "Refile")
      "* TODO  "))))
 '(org-enforce-todo-dependencies t)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m org-bullets org-checklist org-jira org-panel org-registry org-velocity)))
 '(org-refile-targets (quote ((org-agenda-files :tag . ""))))
 '(org-support-shift-select t)
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "STARTED(s@/!)" "|" "DONE(d!)" "CANCELED(c@)")
     (sequence "BUY(b)" "ORDERED(o@)" "|" "RECIEVED(a!)" "RETURNED(r@)")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:inherit error :underline (:color "color-88" :style wave)))))
 '(flycheck-info ((t (:inherit success :underline (:color foreground-color :style wave))))))

;; Theme
(load-theme 'badwolf t)

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

;; Powerline
(require 'spaceline-config)
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)
(spaceline-emacs-theme)

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-M-g") 'helm-google)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;; TRAMP
(setq tramp-default-method "ssh")

;; multi-term
(require 'multi-term)
(setq multi-term-program "/Users/bwarren/.local/bin/zsh")

;; js3-mode
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js3-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js3-mode))

;; magit-gh-pulls
(require 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;; company-mode
(add-hook 'after-init-hook 'global-company-mode)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)


;; any-ini-mode
(require 'any-ini-mode)
(add-to-list 'auto-mode-alist '(".*\\.ini$" . any-ini-mode))
(add-to-list 'auto-mode-alist '(".*\\.conf$" . any-ini-mode))
