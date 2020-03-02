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

(use-package rust-mode :ensure t)
(use-package lsp-haskell :ensure t)

(use-package lsp-mode
  :ensure t
  :hook (python-mode go-mode rust-mode)
  :bind (:map lsp-mode-map
			  ("C-c l a" . lsp-execute-code-action)
			  ("C-c l r" . lsp-rename))
  :config
  (setq lsp-clients-go-imports-local-prefix "ben")
  (setq lsp-auto-guess-root t)
  (setq lsp-clients-python-settings
        '(
          :configurationSources ("flake8")
          :plugins\.jedi_completion\.enabled t
          :plugins\.jedi_definition\.enabled t
          :plugins\.jedi_definition\.follow_builtin_imports nil
          :plugins\.jedi_definition\.follow_imports nil
          :plugins\.jedi_definition\.follow_imports t
          :plugins\.jedi_hover\.enabled t
          :plugins\.jedi_references\.enabled t
          :plugins\.jedi_signature_help\.enabled nil
          :plugins\.jedi_symbols\.all_scopes t
          :plugins\.jedi_symbols\.enabled nil
          :plugins\.mccabe\.enabled nil
          :plugins\.mccabe\.threshold 15
          :plugins\.preload\.enabled true
          :plugins\.preload\.modules nil
          :plugins\.pycodestyle\.enabled nil
          :plugins\.pycodestyle\.exclude nil
          :plugins\.pycodestyle\.filename nil
          :plugins\.pycodestyle\.hangClosing nil
          :plugins\.pycodestyle\.ignore nil
          :plugins\.pycodestyle\.maxLineLength nil
          :plugins\.pycodestyle\.select nil
          :plugins\.pydocstyle\.addIgnore nil
          :plugins\.pydocstyle\.addSelect nil
          :plugins\.pydocstyle\.convention nil
          :plugins\.pydocstyle\.enabled t
          :plugins\.pydocstyle\.ignore nil
          :plugins\.pydocstyle\.match "(?!test_).*\\.py"
          :plugins\.pydocstyle\.matchDir nil
          :plugins\.pydocstyle\.select nil
          :plugins\.pyflakes\.enabled t
          :plugins\.pyls_mypy\.enabled nil
          :plugins\.rope_completion\.enabled t
          :plugins\.yapf\.enabled t
          :rope\.extensionModules nil
          :rope\.ropeFolder nil)))

(use-package lsp-java
  :ensure t
  :after lsp
  :hook (java-mode . lsp)
  :config
  (defvar bewarre/lsp-java-lombok-jar-location "/code/ext/lombok.jar")
  (add-to-list 'lsp-java-vmargs (format "-javaagent:%s" bewarre/lsp-java-lombok-jar-location))
  (add-to-list 'lsp-java-vmargs (format "-Xbootclasspath/a:%s" bewarre/lsp-java-lombok-jar-location))
)

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package dap-java :after (lsp-java))

(use-package dap-mode
  :ensure t
  :after lsp
  :config
  (dap-mode 1)
  (dap-ui-mode 1))

(use-package go-mode
  :ensure t
  :after lsp
  :init
  (setenv "GOPATH" "/code/go" t)
  (setenv "PATH" (concat (getenv "PATH") ":" "$GOPATH/bin") t)
  (setq exec-path (append exec-path (list (expand-file-name
                                           "/code/go/bin"))))
  (add-hook 'go-mode-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
  (add-hook 'go-mode-hook '(lambda () (setq tab-width 4)))
  (add-hook 'go-mode-hook #'lsp-deferred)
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

(use-package helm-lsp
  :ensure t
  :after (helm lsp-mode))


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
  :bind (("C-c h s s" . 'helm-swoop)
         ("C-c h s b" . 'helm-swoop-back-to-last-point)
         ("C-c h s m" . 'helm-multi-swoop)
         ("C-c h s a" . 'helm-multi-swoop-all)
         :map isearch-mode-map
         ("M-i" . helm-swoop-from-isearch)
         :map helm-swoop-map
         ("M-i" . helm-multi-swoop-all-from-helm-swoop)))

(use-package company-lsp
  :ensure t
  :after (company lsp-mode)
  :commands company-lsp
  :config
  (add-to-list 'company-backends 'company-lsp)
  )

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-mode . flycheck-mode))
  :commands lsp-ui-mode
  :config
  (setq lsp-prefer-flymake nil)
  )

(use-package flycheck-rust
  :ensure t
  :after (rust-mode flycheck)
  :hook ((flycheck-mode . flycheck-rust-setup)
         (rust-mode . flycheck-mode))
  )

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets :ensure t :after yasnippet)

(use-package treemacs
  :ensure t
  :config
  (global-set-key (kbd "C-c t") 'treemacs)
  )
(use-package treemacs-projectile :ensure t :after (treemacs))
(use-package lsp-treemacs :ensure t :after (lsp-mode treemacs))

(setq compilation-scroll-output t)

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))
