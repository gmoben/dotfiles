(use-package any-ini-mode
  :load-path "../.emacs.conf/lisp"
  :config
  (add-to-list 'auto-mode-alist '(".*\\.ini$" . any-ini-mode))
  (add-to-list 'auto-mode-alist '(".*\\.conf$" . any-ini-mode)))

(use-package pyvenv
  :after python-mode
  :commands gmoben/py-auto-lsp
  :bind (:map python-mode-map
              ("C-c C-a" . gmoben/py-auto-lsp))
  :hook (python-mode . gmoben/py-auto-lsp)
  :init
  (setenv "WORKON_HOME" (substitute-in-file-name "$HOME/.pyenv/versions"))
  (defun gmoben/py-workon-project-venv ()
    "Call pyenv-workon with the current projectile project name.
This will return the full path of the associated virtual
environment found in $WORKON_HOME, or nil if the environment does
not exist."
    (let ((pname (projectile-project-name)))
      (pyvenv-workon pname)
      (if (file-directory-p pyvenv-virtual-env)
          pyvenv-virtual-env
        (pyvenv-deactivate))))

  (defun gmoben/py-auto-lsp ()
    "Turn on lsp mode in a Python project with some automated logic.
Try to automatically determine which pyenv virtual environment to
activate based on the project name, using
`dd/py-workon-project-venv'. If successful, call `lsp'. If we
cannot determine the virtualenv automatically, first call the
interactive `pyvenv-workon' function before `lsp'"
    (interactive)
    (let ((pvenv (gmoben/py-workon-project-venv)))
      (if pvenv
          (lsp)
        (progn
          (call-interactively #'pyvenv-workon)
          (lsp)))))
  )

(use-package python-docstring
  :after python-mode
  :hook (python-mode . python-docstring-mode))

(use-package rust-mode
  :hook (rust-mode . lsp)
  :init
  (defvar gmoben/cargo_bin (substitute-in-file-name "$HOME/.cargo/bin"))
  (setenv "PATH" (concat (getenv "PATH") ":" gmoben/cargo_bin))
  (setq exec-path (append exec-path (list gmoben/cargo_bin))))

(use-package cargo :hook (rust-mode . cargo-minor-mode))

(use-package lsp-haskell)

(use-package lsp-mode
  :after projectile
  :commands lsp
  :hook
  (sh-mode . lsp)
  :bind (:map lsp-mode-map
              ("C-c l a" . lsp-execute-code-action)
              ("C-c l r" . lsp-rename))
  :init
  (require 'lsp-clients)
  (setq lsp-completion-enable-additional-text-edit nil)
  (setq lsp-clients-go-imports-local-prefix "ben")
  (setq lsp-auto-guess-root t)
  (setq lsp-pyls-configuration-sources ["flake8"])
  (setq lsp-pyls-plugins-pycodestyle t)
  (setq lsp-pyls-plugins-pycodestyle-hang-closing t)
  (setq lsp-pyls-plugins-pycodestyle-max-line-length 120)
  (setq lsp-pyls-plugins-pydocstyle-enabled nil)
  (setq lsp-pyls-plugins-flake8-hang-closing t)
  (setq lsp-pyls-plugins-flake8-max-line-lenth 120)
  (setq lsp-pyls-plugins-jedit-use-pyenv-environment t))

(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp)
  ;; (defvar gmoben/lsp-java-lombok-jar-location "/code/ext/lombok.jar")
  ;; (add-to-list 'lsp-java-vmargs (format "-javaagent:%s" gmoben/lsp-java-lombok-jar-location))
  ;; (add-to-list 'lsp-java-vmargs (format "-Xbootclasspath/a:%s" gmoben/lsp-java-lombok-jar-location))
)

(use-package dap-mode
  :config (dap-auto-configure-mode))

(use-package dap-java :ensure nil)

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

(use-package company
  :after (company-go)
  :hook ((after-init . global-company-mode))
  :config
  (add-to-list 'company-backends 'company-jedi)
  (add-to-list 'company-backends 'company-go)
  :bind (:map company-mode-map
         ("C-:" . helm-company)
         :map company-active-map
         ("C-:" . helm-company)
         ("C-n" . company-select-next-or-abort)
         ("C-p" . company-select-previous-or-abort)))

(use-package flycheck
  :after (helm lsp-ui)
  :hook ((after-init . global-flycheck-mode))
  :bind (:map flycheck-mode-map
              ("C-c ! h" . helm-flycheck))
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package helm
  :init (setq helm-M-x-fuzzy-match t)
  :config
  ;; Remapped bindings
  (global-set-key [remap execute-extended-command] 'helm-M-x)
  (global-set-key [remap find-file] 'helm-find-files)
  (global-set-key [remap list-buffers] 'helm-buffers-list)
  (global-set-key [remap yank-pop] 'helm-show-kill-ring)

  (global-set-key (kbd "C-c s") 'helm-semantic-or-imenu))

(use-package helm-lsp
  :after (helm lsp-mode))

(use-package helm-ag
  :after (helm)
  :init (setq helm-ag-base-command "ag -f --nocolor --nogroup --hidden"))

(use-package helm-company :after (helm company))

(use-package helm-descbinds
  :after (helm)
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

(use-package company-lsp
  :after (company lsp-mode)
  :commands company-lsp
  :config
  (add-to-list 'company-backends 'company-lsp)
  )

(use-package lsp-ui
  :after (lsp-mode)
  :hook ((lsp-ui-mode . lsp)
         (flycheck-mode . lsp))
  :commands lsp-ui-mode
  :config
  (setq lsp-prefer-flymake nil)
  )

(use-package flycheck-rust
  :after (rust-mode flycheck)
  :hook ((flycheck-mode . flycheck-rust-setup)
         (rust-mode . flycheck-mode))
  )

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets :ensure t :after yasnippet)

(use-package treemacs
  :config
  (global-set-key (kbd "C-c t") 'treemacs))
(use-package treemacs-projectile :after (treemacs))
(use-package lsp-treemacs :after (lsp-mode treemacs))

(setq compilation-scroll-output t)

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package neotree :after all-the-icons)

(use-package paradox)
