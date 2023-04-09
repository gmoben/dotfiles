;; Many suggestions adapted from http://doc.norang.ca/org-mode.html

(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-clock)
(require 'ox) ;; org-export http://orgmode.org/worg/exporters/ox-docstrings.html


;; General
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|orgtmpl\\)$" . org-mode))
(add-to-list 'org-modules 'org-habit)
(setq org-startup-indented t)
(setq org-export-coding-system 'utf-8)
(setq org-directory "/code/org/")
(setq org-default-notes-file (concat org-directory "work/incoming.org"))
(setq org-outline-path-complete-in-steps t)

;; Folder and file location variables
;; TODO: Generate these
(defconst ben/org/ben (concat org-directory "ben/"))
(defconst ben/org/ben/stories (concat ben/org/ben "stories.org"))
(defconst ben/org/ben/incoming (concat ben/org/ben "incoming.org"))

(defconst ben/org/work (concat org-directory "work/"))
(defconst ben/org/work/stories (concat ben/org/work "stories.org"))
(defconst ben/org/work/incoming (concat ben/org/work "incoming.org"))

;; Clock
(org-clock-persistence-insinuate)
(setq org-clock-persist 'history)

;; Todo
(setq org-log-into-drawer t)
(setq org-log-reschedule 'time) ;; Add annotations when a task is rescheduled
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-use-fast-todo-selection t) ;; C-c C-t <shortcut key>
(setq org-treat-S-cursor-todo-selection-as-state-change nil) ;; Use S-<left> and S-<right> to cycle TODO states without ! or @
(setq org-todo-keywords
      '((sequence "TODO(t)" "BLOCKED(b@/@)" "|" "DONE(d!)" "CANCELED(c@/@)"))) ;; General


;; Refile
(defun ben/org/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'ben/org/verify-refile-target)
(setq org-refile-use-outline-path t) ;; Show full paths when refiling
(setq org-refile-allow-creating-parent-nodes 'confirm) ;; Allow refile to create parent tasks with confirmation
(setq org-refile-targets '((nil :maxlevel . 9)
               (org-agenda-files :maxlevel . 9)))


;; ;; Capture
;; ;; TODO: Generate these
;; (defvar ben/org/capture/ben
;;   '(("b" "Ben (Personal)")
;;     ("bw" "Story"
;;      entry
;;      (file ben/org/ben/stories)
;;      (file "~/.emacs.conf/org-templates/story.orgtmpl")
;;      :clock-in t :clock-resume t)
;;     ("bt" "Todo"
;;      entry
;;      (file+headline ben/org/ben/incoming "Todos")
;;      (file "~/.emacs.conf/org-templates/todo.orgtmpl")
;;      :clock-in t :clock-resume t)
;;     ("bh" "Habit"
;;      entry
;;      (file+headline ben/org/ben/incoming "Habits")
;;      (file "~/.emacs.conf/org-templates/habit.orgtmpl")
;;      :clock-in t :clock-resume t)
;;     ))

;; (defvar ben/org/capture/work
;;   '(("w" "Work")
;;     ("ws" "Story"
;;      entry
;;      (file ben/org/work/stories)
;;      (file "~/.emacs.conf/org-templates/story.orgtmpl")
;;      :clock-in t :clock-resume t)
;;     ("wt" "Todo"
;;      entry
;;      (file+headline ben/org/ben/incoming "Todos")
;;      (file "~/.emacs.conf/org-templates/todo.orgtmpl")
;;      :clock-in t :clock-resume t)
;;     ("wh" "Habit"
;;      entry
;;      (file+headline ben/org/ben/incoming "Habits")
;;      (file "~/.emacs.conf/org-templates/habit.orgtmpl")
;;      :clock-in t :clock-resume t)
;; ))

;; ;; Agenda
;; (setq org-agenda-compact-blocks t)
;; (setq org-agenda-files (list))
;; (dolist (path '("/code/org/ben" "/code/org/work") nil)
;;   (if (or (file-exists-p path) (file-symlink-p path))
;;       (add-to-list 'org-agenda-files path)))

;; ;; Conditionally set capture templates
;; (setq org-capture-templates
;;       (let (tmpl (list))
;;     (if (member "/code/org/ben" org-agenda-files)
;;         (dolist (elem ben/org/capture/ben)
;;           (add-to-list 'tmpl elem 'append)))

;;     (if (member "/code/org/work" org-agenda-files)
;;         (dolist (elem ben/org/capture/ben)
;;           (add-to-list 'tmpl elem 'append)))
;;     tmpl))


;; (setq org-agenda-custom-commands
;;       '(("h" "Habits" tags-todo "STYLE=\"habit\""
;;      ((org-agenda-overriding-header "Habits")
;;       (org-agenda-sorting-strategy
;;        '(todo-state-down effort-up category-keep))
;;       ))
;;     (" " "Agenda"
;;      ((agenda "" nil)
;;       (tags "REFILE+CREATED={.+}"
;;         ((org-agenda-overriding-header "Refile")
;;          (org-tags-match-list-sublevels t)
;;          ))
;;       (todo "NEXT"
;;              ((org-agenda-overriding-header "Next")
;;               ))

          ;; ))
    ;; ("N" "Notes" tags "NOTE"
    ;;  ((org-agenda-overriding-header "Notes")
    ;;   (org-tags-match-list-sublevels t)))
    ;; ))


;; Global Key Bindings
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o b") 'org-iswitchb) ;; TODO Find/write a helm plugin
(global-set-key (kbd "C-c o c") 'org-capture)

;; (global-set-key (kbd "C-c o d") (lambda() (interactive) (find-file org-directory)))
;; (global-set-key (kbd "C-c o s") (lambda() (interactive) (find-file ben/org/ben/stories)))
;; (global-set-key (kbd "C-c o n") (lambda() (interactive) (find-file ben/org/ben/snippets)))
;; (global-set-key (kbd "C-c o w s") (lambda() (interactive) (find-file ben/org/work/stories)))
;; (global-set-key (kbd "C-c o w n") (lambda() (interactive) (find-file ben/org/work/snippets)))

;; Org-specific bindings

;; Headings
(define-key org-mode-map (kbd "C-c o t") 'org-insert-todo-heading)
(define-key org-mode-map (kbd "C-c o h") 'org-insert-heading-after-current)
(define-key org-mode-map (kbd "C-c o s") (lambda ()
                       (interactive)
                       (end-of-line)
                       (org-insert-subheading nil))) ;; Insert after current line

;; Headers with timestamps (useful for meeting minutes)
(defun org-insert-heading-with-timestamp ()
  (interactive)
  (org-insert-heading-after-current)
  (let ((current-prefix-arg '(16))) (call-interactively #'org-time-stamp))
  (insert " "))

(defun org-insert-heading-with-timestamp-inactive ()
  (interactive)
  (org-insert-heading-after-current)
  (let ((current-prefix-arg '(16))) (call-interactively #'org-time-stamp-inactive)))

(define-key org-mode-map (kbd "C-c h .") 'org-insert-heading-with-timestamp)
(define-key org-mode-map (kbd "C-c h !") 'org-insert-heading-with-timestamp-inactive)

;; Properties
(define-key org-mode-map (kbd "C-c o p s") 'org-set-property)
(define-key org-mode-map (kbd "C-c o p d") 'org-delete-property)

;; Set any property with a timestamps
(defun org-set-property-with-timestamp (property)
  (interactive (list nil))
  (let ((property (or property (org-read-property-name))))
    (unless (org--valid-property-p property)
      (user-error "Invalid property name: \"%s\"" property))
    (let (timestamp)
      (with-temp-buffer
        (org-mode) ; Enable org-mode to use org-time-stamp
        (call-interactively 'org-time-stamp)
        (setq timestamp (buffer-string)))
      (org-set-property property timestamp))))

(defun org-set-property-with-timestamp-inactive (property)
  (interactive (list nil))
  (let ((property (or property (org-read-property-name))))
    (unless (org--valid-property-p property)
      (user-error "Invalid property name: \"%s\"" property))
    (let (timestamp)
      (with-temp-buffer
        (org-mode) ; Enable org-mode to use org-time-stamp
        (call-interactively 'org-time-stamp-inactive)
        (setq timestamp (buffer-string)))
      (org-set-property property timestamp))))

(define-key org-mode-map (kbd "C-c o p .") 'org-set-property-with-timestamp)
(define-key org-mode-map (kbd "C-c o p !") 'org-set-property-with-timestamp-inactive)

;; Unmap add/remove files from org-agenda-files
(define-key org-mode-map (kbd "C-c [") nil)
(define-key org-mode-map (kbd "C-c ]") nil)

;; Map RET to <f12> for iTerm
(define-key org-mode-map (kbd "<C-return>") 'org-insert-heading-respect-content)
(define-key org-mode-map (kbd "<C-^>") 'org-insert-heading-respect-content)
(define-key org-mode-map (kbd "<f12>") 'org-return)
(define-key org-mode-map (kbd "<M-f12>") 'org-meta-return)
(define-key org-mode-map (kbd "<C-f12>") 'org-insert-heading-respect-content)
(define-key org-mode-map (kbd "<C-S-f12>") 'org-insert-todo-heading-respect-content)
(define-key org-mode-map (kbd "<M-S-f12>") 'org-insert-todo-heading)

;; (setq org-todo-state-tags-triggers
;;       '(("BLOCKED" ("BLOCKED" . t))

;; ;; Remove empty LOGBOOK drawers on clock out
;; (defun bmw/remove-empty-drawer-on-clock-out ()
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line 0)
;;     (org-remove-empty-drawer-at (point))))

;; (add-hook 'org-clock-out-hook 'bmw/remove-empty-drawer-on-clock-out 'append)

(add-hook 'org-mode-hook 'visual-line-mode)
