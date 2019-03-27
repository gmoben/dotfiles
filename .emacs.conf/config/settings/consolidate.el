(use-package any-ini-mode
  :config
  (add-to-list 'auto-mode-alist '(".*\\.ini$" . any-ini-mode))
  (add-to-list 'auto-mode-alist '(".*\\.conf$" . any-ini-mode))
  )

(use-package python-mode
  :ensure t
  :after (projectile)
  :config
  (defvar my:virtualenv-directory "~/.virtualenvs/"
    "The directory of virtualenvs.")

  (defun my:configure-python-venv ()
    "Set `python-shell-virtualenv-path' to the virtualenv directory."
    (interactive)
    (let* ((project-name (projectile-project-name))
           (virtualenv-path
            (file-truename
             (concat my:virtualenv-directory project-name))))
      (when (file-directory-p virtualenv-path)
        (setq python-shell-virtualenv-path virtualenv-path))))

  (defun my:flycheck-python-set-executables ()
    "Set flycheck python executables for the current virtualenv."
    (let ((exec-path (python-shell-calculate-exec-path)))
      (setq-local flycheck-python-pylint-executable (executable-find "pylint"))
      (setq-local flycheck-python-flake8-executable (executable-find "flake8"))))

  (defun my:flycheck-python-setup ()
    "Setup flycheck for Python with virtualenvs. "
    ;; my:flycheck-python-set-executables uses buffer-local variables
    (add-hook 'hack-local-variables-hook #'my:flycheck-python-set-executables
              nil 'local))

  (add-hook 'python-mode-hook #'my:configure-python-venv)
  (add-hook 'python-mode-hook #'my:flycheck-python-setup)
  )

(use-package lsp-mode
  :ensure t
  :after (rust-mode go-mode python-mode)
  :hook ((rust-mode . lsp)
         (go-mode . lsp)
         (python-mode . lsp))
  :config
  (setq lsp-clients-go-imports-local-prefix "github.atl.pdrop.net")
  )

(use-package go-mode
  :ensure t
  :init
  (setenv "GOPATH" "/code/go" t)
  (setenv "PATH" (concat (getenv "PATH") ":" "$GOPATH/bin") t)
  (setq exec-path (append exec-path (list (expand-file-name
                                           "/code/go/bin"))))
  (add-hook 'go-mode-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
  (add-hook 'go-mode-hook '(lambda () (setq tab-width 4)))
  :hook ((before-save . gofmt-before-save))
  :config
  (setq gofmt-command "goimports")
  )

(use-package company
  :ensure t
  :after (company-jedi company-go)
  :hook ((after-init . global-company-mode))
  :config
  (add-to-list 'company-backends 'company-jedi)
  (add-to-list 'company-backends 'company-go)
  :bind (:map company-mode-map
         ("C-:" . helm-company)
         :map company-active-map
         ("C-:" . helm-company)
         ("C-n" . company-select-next-or-abort)
         ("C-p" . company-select-previous-or-abort))
  )

(use-package flycheck
  :ensure t
  :after (helm lsp-ui)
  :hook ((after-init . global-flycheck-mode))
  :bind (:map flycheck-mode-map
              ("C-c ! h" . helm-flycheck))
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  )

(use-package helm
  :ensure t
  :init
  (setq helm-M-x-fuzzy-match t)
  :config
  ;; Remapped bindings
  (global-set-key [remap execute-extended-command] 'helm-M-x)
  (global-set-key [remap find-file] 'helm-find-files)
  (global-set-key [remap list-buffers] 'helm-buffers-list)
  (global-set-key [remap yank-pop] 'helm-show-kill-ring)

  (global-set-key (kbd "C-c s") 'helm-semantic-or-imenu)
  )

(use-package helm-ag
  :ensure t
  :after (helm)
  :init
  (setq helm-ag-base-command "ag -f --nocolor --nogroup --hidden")
  )

(use-package helm-company
  :ensure t
  :after (helm company)
  )

(use-package helm-descbinds
  :ensure t
  :after (helm)
  :config
  (helm-descbinds-mode)
  )

(use-package helm-describe-modes
  :ensure t
  :after (helm)
  :config
  (global-set-key [remap describe-mode] 'helm-describe-modes)
  )

(use-package helm-google
  :ensure t
  :after (helm)
  :config
  (global-set-key (kbd "C-c h g") 'helm-google)
  )

(use-package helm-org-rifle
  :ensure t
  :after (helm org)
  :config
  (global-set-key (kbd "C-c o r a") 'helm-org-rifle-agenda-files)
  (global-set-key (kbd "C-c o r b") 'helm-org-rifle-current-buffer)
  (global-set-key (kbd "C-c o r d") 'helm-org-rifle-directories)
  (global-set-key (kbd "C-c o r f") 'helm-org-rifle-files)
  (global-set-key (kbd "C-c o r r") 'helm-org-rifle-org-directory)
  )

(use-package helm-swoop
  :ensure t
  :after (helm)
  :init
  (setq helm-swoop-split-with-multiple-windows t)
  (setq helm-swoop-use-fuzzy-match t)
  :bind (:map isearch-mode-map
              ("M-i" . helm-swoop-from-isearch)
         :map helm-swoop-map
              ("M-i" . helm-multi-swoop-all-from-helm-swoop))
  :config
  (global-set-key (kbd "C-c h s s") 'helm-swoop)
  (global-set-key (kbd "C-c h s b") 'helm-swoop-back-to-last-point)
  (global-set-key (kbd "C-c h s m") 'helm-multi-swoop)
  (global-set-key (kbd "C-c h s a") 'helm-multi-swoop-all)
)

(use-package company-lsp
  :ensure t
  :after (company lsp-mode)
  :config
  (add-to-list 'company-backends 'company-lsp)
  )


(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-mode . flycheck-mode))
  :config
  (setq lsp-prefer-flymake nil)
  )

(use-package flycheck-rust
  :ensure t
  :after (rust-mode flycheck)
  :hook ((flycheck-mode . flycheck-rust-setup)
         (rust-mode . flycheck-mode))
  )
