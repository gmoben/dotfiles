;; Many suggestions adapted from http://doc.norang.ca/org-mode.html

(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-clock)
(require 'ox) ;; org-export http://orgmode.org/worg/exporters/ox-docstrings.html

;; Key Bindings
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o b") 'org-iswitchb) ;; TODO Find/write a helm plugin
(global-set-key (kbd "C-c o c") 'org-capture)

(define-key org-mode-map (kbd "C-c o h") 'org-insert-heading-after-current)
(define-key org-mode-map (kbd "C-c o s") (lambda ()
					   (interactive)
					   (end-of-line)
					   (org-insert-subheading nil))) ;; Insert after current line
(define-key org-mode-map (kbd "C-c o t") 'org-insert-todo-heading)
(define-key org-mode-map (kbd "C-c o p s") 'org-set-property)
(define-key org-mode-map (kbd "C-c o p d") 'org-delete-property)

;; Unmap add/remove files from org-agenda-files
(define-key org-mode-map (kbd "C-c [") nil)
(define-key org-mode-map (kbd "C-c ]") nil)

;; Map RET to <f12> for iTerm
(define-key org-mode-map (kbd "<C-^>") 'org-insert-heading-respect-content)
(define-key org-mode-map (kbd "<f12>") 'org-return)
(define-key org-mode-map (kbd "<M-f12>") 'org-meta-return)
(define-key org-mode-map (kbd "<C-f12>") 'org-insert-heading-respect-content)
(define-key org-mode-map (kbd "<C-S-f12>") 'org-insert-todo-heading-respect-content)
(define-key org-mode-map (kbd "<M-S-f12>") 'org-insert-todo-heading)

;; Settings
(org-clock-persistence-insinuate)
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|orgtmpl\\)$" . org-mode))
(setq org-startup-indented t)
(setq org-export-coding-system 'utf-8)
(setq org-log-reschedule 'time) ;; Add annotations when a task is rescheduled
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-use-fast-todo-selection t) ;; C-c C-t <shortcut key>
(setq org-treat-S-cursor-todo-selection-as-state-change nil) ;; Use S-<left> and S-<right> to cycle TODO states without ! or @
(setq org-clock-persist 'history)
(setq org-refile-use-outline-path t) ;; Show full paths when refiling
(setq org-refile-allow-creating-parent-nodes 'confirm) ;; Allow refile to create parent tasks with confirmation
(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "refile.org"))

(setq org-agenda-files (list))
(dolist (folder '("ben/" "work/") nil)
  (let (path)
    (setq path (concat org-directory folder))
    (if (or (file-exists-p path) (file-symlink-p path))
	(add-to-list 'org-agenda-files path))))

(setq org-refile-targets '((nil :maxlevel . 9)
			   (org-agenda-files :maxlevel . 9)))

(defvar ben/org-templates/standalone
  '(("b" "Buy"
     entry
     (file+headline "~/org/ben/refile.org" "Shopping")
     (file "~/.emacs.conf/org-templates/buy.orgtmpl")
     :clock-in t :clock-resume t)
    ("q" "Quest"
     entry
     (file+headline "~/org/ben/quests.org" "Incoming")
     (file "~/.emacs.conf/org-templates/quest.orgtmpl")
     :clock-in t :clock-resume t)))

(defvar ben/org-templates/general
  ;; General
  '(("p" "Personal")
    ("pt" "Todo"
     entry
     (file+headline "~/org/ben/refile.org" "Tasks")
     (file "~/.emacs.conf/org-templates/todo.orgtmpl")
     :clock-in t :clock-resume t)
    ("pi" "Project Idea"
     entry
     (file+headline "~/org/ben/refile.org" "Ideas")
     (file "~/.emacs.conf/org-templates/idea.orgtmpl")
     :clock-in t :clock-resume t)))

(defvar ben/org-templates/work
  ;; Work-specific
  '(("w" "Work")
    ("wt" "Todo"
     entry
     (file+headline "~/org/work/refile.org" "Tasks")
     (file "~/.emacs.conf/org-templates/todo.orgtmpl")
     :clock-in t :clock-resume t)
    ("wi" "Project Idea"
     entry
     (file+headline "~/org/work/refile.org" "Ideas")
     (file "~/.emacs.conf/org-templates/idea.orgtmpl")
     :clock-in t :clock-resume t)))

(setq org-capture-templates
      (let (tmpl (list))
	(if (member "~/org/ben/" org-agenda-files)
	    (dolist (group (list ben/org-templates/standalone ben/org-templates/general))
	      (dolist (elem group)
		(add-to-list 'tmpl elem 'append))))

	(if (member "~/org/work/" org-agenda-files)
	    (dolist (elem ben/org-templates/work)
	      (add-to-list 'tmpl elem 'append)))
	tmpl))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "BLOCKED(b@/@)" "|" "DONE(d!)" "CANCELED(c@/@)") ;; General
	(sequence "BUY($!)" "|" "PURCHASED(p@)" "ARRIVED(a@)") ;; Grocery, Amazon, etc.
	(sequence "DESIGN(D/@)" "IMPLEMENT(I/@)" "REFACTOR(R/@)" "|" "MERGED(M!/@)" "CLOSED(C@/@)"))) ;; Projects

; Exclude DONE state tasks from refile targets
(defun ben/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'ben/verify-refile-target)

;; (setq org-todo-state-tags-triggers
;;       '(("BLOCKED" ("BLOCKED" . t))

;; ;; Remove empty LOGBOOK drawers on clock out
;; (defun bmw/remove-empty-drawer-on-clock-out ()
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line 0)
;;     (org-remove-empty-drawer-at (point))))

;; (add-hook 'org-clock-out-hook 'bmw/remove-empty-drawer-on-clock-out 'append)
