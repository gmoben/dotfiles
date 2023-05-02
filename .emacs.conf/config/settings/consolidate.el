(use-package auto-minor-mode)

(use-package any-ini-mode
  :load-path "../.emacs.conf/lisp"
  :config
  (add-to-list 'auto-mode-alist '(".*\\.ini$" . any-ini-mode))
  (add-to-list 'auto-mode-alist '(".*\\.conf$" . any-ini-mode)))

;; (use-package pyvenv
;;   :after python-mode
;;   :commands gmoben/py-auto-lsp
;;   :bind (:map python-mode-map
;;               ("C-c C-a" . gmoben/py-auto-lsp))
;;   :hook (python-mode . gmoben/py-auto-lsp)
;;   :init
;;   (setenv "WORKON_HOME" (substitute-in-file-name "$HOME/.pyenv/versions"))
;;   (defun gmoben/py-workon-project-venv ()
;;     "Call pyenv-workon with the current projectile project name.
;; This will return the full path of the associated virtual
;; environment found in $WORKON_HOME, or nil if the environment does
;; not exist."
;;     (let ((pname (projectile-project-name)))
;;       (pyvenv-workon pname)
;;       (if (file-directory-p pyvenv-virtual-env)
;;           pyvenv-virtual-env
;;         (pyvenv-deactivate))))

;;   (defun gmoben/py-auto-lsp ()
;;     "Turn on lsp mode in a Python project with some automated logic.
;; Try to automatically determine which pyenv virtual environment to
;; activate based on the project name, using
;; `dd/py-workon-project-venv'. If successful, call `lsp'. If we
;; cannot determine the virtualenv automatically, first call the
;; interactive `pyvenv-workon' function before `lsp'"
;;     (interactive)
;;     (let ((pvenv (gmoben/py-workon-project-venv)))
;;       (if pvenv
;;           (lsp)
;;         (progn
;;           (call-interactively #'pyvenv-workon)
;;           (lsp)))))
;;   )

(use-package python-docstring
  :after python-mode
  :hook (python-mode . python-docstring-mode))

(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c l l" . flycheck-list-errors)
              ("C-c l s" . lsp-rust-analyzer-status))
  :init
  (defvar gmoben/cargo_bin (substitute-in-file-name "$HOME/.cargo/bin"))
  (setenv "PATH" (concat (getenv "PATH") ":" gmoben/cargo_bin))
  (setq exec-path (append exec-path (list gmoben/cargo_bin)))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))


(use-package cargo :hook (rustic-mode . cargo-minor-mode))

(use-package lsp-haskell)

(use-package lsp-mode
  :after projectile
  :commands lsp
  :hook ((sh-mode . lsp)
         (ruby-mode . lsp)
         (java-mode . lsp)
         (rustic-mode . lsp))
  :bind (:map lsp-mode-map
              ("C-c l a" . lsp-execute-code-action)
              ("C-c l r" . lsp-rename)
              ("C-c l q" . lsp-workspace-restart)
              ("C-c l Q" . lsp-workspace-shutdown))
  :init
  ;; (setq lsp-pyls-configuration-sources ["flake8"])
  ;; (setq lsp-pyls-plugins-pycodestyle t)
  ;; (setq lsp-pyls-plugins-pycodestyle-hang-closing t)
  ;; (setq lsp-pyls-plugins-pycodestyle-max-line-length 120)
  ;; (setq lsp-pyls-plugins-pydocstyle-enabled nil)
  ;; (setq lsp-pyls-plugins-flake8-hang-closing t)
  ;; (setq lsp-pyls-plugins-flake8-max-line-lenth 120)
  ;; (setq lsp-pyls-plugins-jedit-use-pyenv-environment t)
  :custom
  (lsp-completion-enable-additional-text-edit nil)
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-auto-guess-root t)
  (lsp-headerline-breadcrumb-enable nil)

  ;; Go
  (lsp-clients-go-imports-local-prefix "ben")

  ;; Rust
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints nil)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints t)
  (lsp-rust-analyzer-display-reborrow-hints t)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-doc-enable t))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; auto-completion and code snippets

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  :ensure
  :bind
  (:map company-active-map
              ("C-n". company-select-next)
              ("C-p". company-select-previous)
              ("M-<". company-select-first)
              ("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete)))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(use-package lsp-java
  :config
  (defvar gmoben/lsp-java-lombok-jar-location "/code/ext/lombok.jar")
  (add-to-list 'lsp-java-vmargs (format "-javaagent:%s" gmoben/lsp-java-lombok-jar-location))
  (add-to-list 'lsp-java-vmargs (format "-Xbootclasspath/a:%s" gmoben/lsp-java-lombok-jar-location)))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

;; Mode-specific flycheck chaining for mutli-mode lsp checker
;; https://github.com/flycheck/flycheck/issues/1762
(use-package flycheck
  :hook ((after-init . global-flycheck-mode))
  :preface
  (defvar-local flycheck-local-checkers nil)
  (defun +flycheck-checker-get(fn checker property)
    (or (alist-get property (alist-get checker flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around '+flycheck-checker-get)
  :bind (:map flycheck-mode-map
              ("C-c ! h" . helm-flycheck))
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package python-mode
  :hook
  (python-mode . (lambda () (setq flycheck-local-checkers '((lsp . ((next-checkers . (python-flake8 python-pylint)))))))))

(use-package dap-mode
  :commands dap-mode
  :config
  (dap-mode 1)
  (require 'dap-ui)
  (dap-ui-mode 1)
  (require 'dap-lldb))

(use-package dap-java :straight nil)

(use-package go-mode
  :init
  (setenv "GOPATH" "/code/go" t)
  (setenv "PATH" (concat (getenv "PATH") ":" "$GOPATH/bin") t)
  (setq exec-path (append exec-path (list (expand-file-name "/code/go/bin"))))
  (add-hook 'go-mode-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
  (add-hook 'go-mode-hook '(lambda () (setq tab-width 4)))
  (add-hook 'go-mode-hook #'lsp-deferred)
  :hook ((before-save . gofmt-before-save))
  :config
  (setq gofmt-command "goimports")
  )

;; (use-package company
;;   :after (company-go)
;;   :hook ((after-init . global-company-mode))
;;   :config
;;   (add-to-list 'company-backends 'company-jedi)
;;   (add-to-list 'company-backends 'company-go)
;;   :bind (:map company-mode-map
;;          ("C-:" . helm-company)
;;          :map company-active-map
;;          ("C-:" . helm-company)
;;          ("C-n" . company-select-next-or-abort)
;;          ("C-p" . company-select-previous-or-abort)))

(use-package helm
  :init (setq helm-M-x-fuzzy-match t)
  :custom
  (helm-completion-style 'emacs)
  :config
  ;; Remapped bindings
  (global-set-key [remap execute-extended-command] 'helm-M-x)
  (global-set-key [remap find-file] 'helm-find-files)
  (global-set-key [remap list-buffers] 'helm-buffers-list)
  (global-set-key [remap yank-pop] 'helm-show-kill-ring)

  (global-set-key (kbd "C-c s") 'helm-semantic-or-imenu)
  (setq completion-styles `(basic partial-completion emacs22 initials
                                  ,(if (version<= emacs-version "27.0") 'helm-flex 'flex)))
  )

(use-package helm-lsp
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
  (define-key lsp-mode-map [remap lsp-execute-code-action] #'helm-lsp-code-actions))

(use-package helm-ag
  :after helm
  :init (setq helm-ag-base-command "ag -f --nocolor --nogroup --hidden"))

(use-package helm-company :after (helm company))

(use-package helm-descbinds
  :after helm
  :config (helm-descbinds-mode))

(use-package helm-describe-modes

  :after (helm)
  :config (global-set-key [remap describe-mode] 'helm-describe-modes))

(use-package helm-org-rifle
  :after (helm org)
  :bind
  ("C-c o r a" . 'helm-org-rifle-agenda-files)
  ("C-c o r b" . 'helm-org-rifle-current-buffer)
  ("C-c o r d" . 'helm-org-rifle-directories)
  ("C-c o r f" . 'helm-org-rifle-files)
  ("C-c o r r" . 'helm-org-rifle-org-directory))

(use-package helm-swoop
  :after (helm)
  :init
  (setq helm-swoop-split-with-multiple-windows t)
  (setq helm-swoop-use-fuzzy-match t)
  :bind (("C-c h s s" . 'helm-swoop)
         ("C-c h s b" . 'helm-swoop-back-to-last-point)
         ("C-c h s m" . 'helm-multi-swoop)
         ("C-c h s a" . 'helm-multi-swoop-all)
         :map isearch-mode-map
         ("M-i" . helm-swoop-from-isearch)
         :map helm-swoop-map
         ("M-i" . helm-multi-swoop-all-from-helm-swoop)))

(use-package helm-tramp)

;; (use-package flycheck-rust
;;   :config
;;   (with-eval-after-load 'rust-mode
;;     (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;; (use-package yasnippet
;;   :config
;;   (yas-global-mode 1))

(use-package yasnippet-snippets :ensure t :after yasnippet)

(use-package treemacs
  :config
  (global-set-key (kbd "C-c t") 'treemacs)
  (treemacs-git-mode 'extended)
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))

(use-package treemacs-projectile :after (treemacs))
(use-package lsp-treemacs :after (lsp-mode treemacs)
  :config
  (lsp-treemacs-sync-mode 1))

(setq compilation-scroll-output t)

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package neotree :after all-the-icons)

(use-package rvm :commands rvm-activate-ruby-for
  :init
  (rvm-use-default))

(use-package helm-org
  :after (helm helm-mode)
  :config
  (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
  (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags)))

(use-package helm-xref)

;; Move buffers between windows
(use-package buffer-move
  :config
  (global-set-key (kbd "<C-S-up>")     'buf-move-up)
  (global-set-key (kbd "<C-S-down>")   'buf-move-down)
  (global-set-key (kbd "<C-S-left>")   'buf-move-left)
  (global-set-key (kbd "<C-S-right>")  'buf-move-right))
