(use-package delight)

(use-package auto-minor-mode)

(use-package any-ini-mode
  :load-path "../.emacs.conf/lisp"
  :config
  (add-to-list 'auto-mode-alist '(".*\\.ini$" . any-ini-mode))
  (add-to-list 'auto-mode-alist '(".*\\.conf$" . any-ini-mode)))

(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode))

(use-package python-docstring
  :after python-mode
  :hook (python-mode . python-docstring-mode))

(use-package rustic
  :bind (:map rustic-mode-map
              ;; ("M-j" . lsp-ui-imenu)
              ;; ("M-?" . lsp-find-references)
              ("C-c l l" . flycheck-list-errors)
              ("C-c l s" . lsp-rust-analyzer-status)
              ("C-c C-c C-a" . rustic-cargo-add)
              ("C-c C-c b" . rustic-cargo-build)
              ("C-c C-c C-c" . rustic-cargo)
              ("C-c C-c C-c" . rustic-cargo-check)
              ("C-c C-c C-d" . rustic-cargo-doc)
              ("C-c C-c C-d" . rustic-docstring-dwim)
              ("C-c C-c C-e" . rustic-cargo-rm)
              ("C-c C-c C-f" . rustic-format-buffer)
              ("C-c C-c C-i" . rustic-cargo-init)
              ("C-c C-c C-k" . rustic-cargo-check-all)
              ("C-c C-c C-l" . rustic-cargo-clippy)
              ("C-c C-c C-m" . rustic-cargo-make)
              ("C-c C-c C-n" . rustic-cargo-new)
              ("C-c C-c C-o" . rustic-cargo-outdated)
              ("C-c C-c C-p" . rustic-cargo-publish)
              ("C-c C-c C-q" . rustic-cargo-check)
              ("C-c C-c C-r" . rustic-cargo-run)
              ("C-c C-c C-s" . rustic-cargo-search)
              ("C-c C-c C-t" . rustic-cargo-test)
              ("C-c C-c C-u" . rustic-cargo-upgrade)
              ("C-c C-c C-v" . rustic-cargo-bench)
              ("C-c C-c C-w" . rustic-cargo-watch)
              ("C-c C-c C-x" . rustic-cargo-run-example)
              ("C-c C-c C-y" . rustic-cargo-hack)
              ("C-c C-c C-z" . rustic-cargo-fmt))
  :init
  (defvar gmoben/cargo_bin (substitute-in-file-name "$HOME/.cargo/bin"))
  (setenv "PATH" (concat (getenv "PATH") ":" gmoben/cargo_bin))
  (setq exec-path (append exec-path (list gmoben/cargo_bin)))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  (setq auto-mode-alist (delete (rassoc 'rust-mode auto-mode-alist) auto-mode-alist))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  ;; (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
  )

;; (defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  ;; (when buffer-file-name
  ;;   (setq-local buffer-save-without-query t))
  ;; (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; (use-package lsp-haskell)

;; (use-package lsp-mode
;;   :after projectile
;;   :commands lsp
;;   :hook ((sh-mode . lsp)
;;          (ruby-mode . lsp)
;;          (java-mode . lsp)
;;          (rustic-mode . lsp))
;;   :bind (:map lsp-mode-map
;;               ("C-c l a" . lsp-execute-code-action)
;;               ("C-c l r" . lsp-rename)
;;               ("C-c l q" . lsp-workspace-restart)
;;               ("C-c l Q" . lsp-workspace-shutdown))
;;   :init
;;   ;; (setq lsp-pyls-configuration-sources ["flake8"])
;;   ;; (setq lsp-pyls-plugins-pycodestyle t)
;;   ;; (setq lsp-pyls-plugins-pycodestyle-hang-closing t)
;;   ;; (setq lsp-pyls-plugins-pycodestyle-max-line-length 120)
;;   ;; (setq lsp-pyls-plugins-pydocstyle-enabled nil)
;;   ;; (setq lsp-pyls-plugins-flake8-hang-closing t)
;;   ;; (setq lsp-pyls-plugins-flake8-max-line-lenth 120)
;;   ;; (setq lsp-pyls-plugins-jedit-use-pyenv-environment t)
;;   :custom
;;   (lsp-completion-enable-additional-text-edit nil)
;;   (lsp-eldoc-render-all t)
;;   (lsp-idle-delay 0.6)
;;   (lsp-auto-guess-root t)
;;   (lsp-headerline-breadcrumb-enable nil)

;;   ;; Go
;;   (lsp-clients-go-imports-local-prefix "ben")

;;   ;; Rust
;;   ;; what to use when checking on-save. "check" is default, I prefer clippy
;;   (lsp-rust-analyzer-cargo-watch-command "clippy")
;;   ;; enable / disable the hints as you prefer:
;;   (lsp-rust-analyzer-server-display-inlay-hints nil)
;;   (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
;;   (lsp-rust-analyzer-display-chaining-hints t)
;;   (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
;;   (lsp-rust-analyzer-display-closure-return-type-hints t)
;;   (lsp-rust-analyzer-display-parameter-hints t)
;;   (lsp-rust-analyzer-display-reborrow-hints t)
;;   :config
;;   (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :custom
;;   (lsp-ui-peek-always-show t)
;;   (lsp-ui-sideline-show-hover t)
;;   (lsp-ui-sideline-show-code-actions t)
;;   (lsp-ui-doc-enable t))

(use-package typescript-mode
  :init
  (add-hook 'typescript-ts-base-mode-hook '(lambda () (setq tab-width 2))))

(use-package kotlin-mode)
;; (use-package kotlin-ts-mode)

(use-package smithy-mode)

(use-package project
  :straight nil
  :ensure nil
  :config
  (add-to-list 'project-vc-extra-root-markers "tsconfig.json"))

(use-package apheleia
  :hook ((typescript-mode . apheleia-mode)
         (typescript-ts-mode . apheleia-mode)))

(use-package flymake
  :ensure nil
  :custom
  (flymake-fringe-indicator-position nil))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package eglot
  :straight nil
  :init
  :defer
  :hook ((bash-mode . eglot-ensure)
         (bash-ts-mode . eglot-ensure)
         (css-mode . eglot-ensure)
         (css-ts-mode . eglot-ensure)
         (java-mode . eglot-ensure)
         (java-ts-mode . eglot-ensure)
         (java-ts-mode . eglot-ensure)
         (javascript-mode . eglot-ensure)
         (js-json-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (js2-mode . eglot-ensure)
         (json-mode . eglot-ensure)
         (json-ts-mode . eglot-ensure)
         (json-ts-mode . eglot-ensure)
         (kotlin-mode . eglot-ensure)
         ;; (kotlin-ts-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (ruby-mode . eglot-ensure)
         (rustic-mode . eglot-ensure)
         (smithy-mode . eglot-ensure)
         (smithy-ts-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         )
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename))
  :config

  (defun eglot-generate-workspace-folders (server)
    "Generate the workspaceFolders value for the workspace.

    This is implemented by returning the content of .bemol/ws_root_folders file"
    (let* ((root (project-root (project-current)))
           (ws-root (file-name-parent-directory
                     (file-name-parent-directory root)))
           (bemol-root (file-name-concat ws-root ".bemol/"))
           (bemol-ws-root-folders (file-name-concat bemol-root "ws_root_folders"))
           (ws-root-folders-content)
           (ws-folders-for-eglot))
      (if (not (file-exists-p bemol-ws-root-folders))
          (eglot-workspace-folders server))
      (setq ws-root-folders-content (with-temp-buffer
                                      (insert-file-contents bemol-ws-root-folders)
                                      (split-string (buffer-string) "\n" t)))
      (setq ws-folders-for-eglot (mapcar (lambda (o) (concat "file://" o))
                                         ws-root-folders-content))
      (vconcat ws-folders-for-eglot)))

  (add-to-list 'eglot-server-programs
               `((java-mode java-ts-mode)
                 . ("jdtls" "--jvm-arg=-javaagent:/code/ext/lombok.jar"
                    ;; The following allows jdtls to find definition
                    ;; if the code lives outside the current project.
                    :initializationOptions
                    ,(lambda (server)
                       `(:workspaceFolders ,(eglot-generate-workspace-folders server)
                                           :extendedClientCapabilities
                                           (:classFileContentsSupport t
                                                                      :overrideMethodsPromptSupport t
                                                                      :hashCodeEqualsPromptSupport t
                                                                      :advancedOrganizeImportsSupport t
                                                                      :generateToStringPromptSupport t
                                                                      :advancedGenerateAccessorsSupport t
                                                                      :generateConstructorsPromptSupport t
                                                                      :generateDelegateMethodsPromptSupport t
                                                                      :advancedExtractRefactoringSupport t
                                                                      :moveRefactoringSupport t
                                                                      :clientHoverProvider t
                                                                      :clientDocumentSymbolProvider t
                                                                      :advancedIntroduceParameterRefactoringSupport t
                                                                      :actionableRuntimeNotificationSupport t
                                                                      :extractInterfaceSupport t
                                                                      :advancedUpgradeGradleSupport t))))))
  (add-to-list 'eglot-server-programs
               '((smithy-mode smithy-ts-mode) . ("smithy-language-server" "0")))

  (defvar eglot-path-uri-cache (make-hash-table :test #'equal)
    "File path to uri cache.")

  (cl-defgeneric +eglot/ext-uri-to-path (uri)
    "Support extension uri."
    nil)

  (define-advice eglot--uri-to-path (:around (orig-fn uri) advice)
    "Support non standard LSP uri scheme."
    (when (keywordp uri) (setq uri (substring (symbol-name uri) 1)))
    (or (+eglot/ext-uri-to-path uri)
        (funcall orig-fn uri)))

  (define-advice eglot--path-to-uri (:around (orig-fn path) advice)
    "Support non standard LSP uri scheme."
    (or (gethash path eglot-path-uri-cache)
        (funcall orig-fn path)))

  (defun +eglot/jdtls-uri-to-path (uri)
    "Support Eclipse jdtls `jdt://' uri scheme."
    (when-let* ((jdt-scheme-p (string-prefix-p "jdt://" uri))
                (filename (when (string-match "^jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri)
                            (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))
                (source-dir (file-name-concat (project-root (eglot--current-project)) ".eglot"))
                (source-file (expand-file-name (file-name-concat source-dir filename))))
      (unless (file-directory-p source-dir)
        (make-directory source-dir t))
      (unless (file-readable-p source-file)
        (let ((content (jsonrpc-request (eglot--current-server-or-lose)
                                        :java/classFileContents
                                        (list :uri uri))))
          (with-temp-file source-file (insert content))))
      (puthash source-file uri eglot-path-uri-cache)
      source-file))

  (cl-defmethod +eglot/ext-uri-to-path (uri &context (major-mode java-mode))
    (+eglot/jdtls-uri-to-path uri))

  (cl-defmethod +eglot/ext-uri-to-path (uri &context (major-mode java-ts-mode))
    (+eglot/jdtls-uri-to-path uri))

  ;; https://github.com/joaotavora/eglot/discussions/888#discussioncomment-2386710
  (cl-defmethod eglot-execute-command
    (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
    "Command `java.apply.workspaceEdit' handler."
    (mapc #'eglot--apply-workspace-edit arguments))

  (cl-defmethod eglot-execute-command
    (_server (_cmd (eql java.action.overrideMethodsPrompt)) arguments)
    "Command `java.action.overrideMethodsPrompt' handler."
    (let* ((argument (aref arguments 0))
           (list-methods-result (jsonrpc-request (eglot--current-server-or-lose)
                                                 :java/listOverridableMethods
                                                 argument))
           (methods (plist-get list-methods-result :methods))
           (menu-items (mapcar (lambda (method)
                                 (let* ((name (plist-get method :name))
                                        (parameters (plist-get method :parameters))
                                        (class (plist-get method :declaringClass)))
                                   (cons (format "%s(%s) class: %s" name (string-join parameters ", ") class) method)))
                               methods))
           (selected-methods (cl-map 'vector
                                     (lambda (choice) (alist-get choice menu-items nil nil 'equal))
                                     (delete-dups
                                      (completing-read-multiple "overridable methods: " menu-items))))
           (add-methods-result (jsonrpc-request (eglot--current-server-or-lose)
                                                :java/addOverridableMethods
                                                (list :overridableMethods selected-methods :context argument))))
      (eglot--apply-workspace-edit add-methods-result)))

  ;; (add-to-list 'eglot-server-programs '((java-mode java-ts-mode) . ("jdtls" "--jvm-arg=-javaagent:/code/ext/lombok.jar")))

  ;; (defun jdtls-command-contact (&optional interactive)
  ;;   (let* ((jdtls-cache-dir (file-name-concat user-emacs-directory "cache" "lsp-cache"))
  ;;          (project-dir (file-name-nondirectory (directory-file-name (project-root (project-current)))))
  ;;          (data-dir (expand-file-name (file-name-concat jdtls-cache-dir (md5 project-dir))))
  ;;          (jvm-args `(,(concat "-javaagent:" (expand-file-name "/code/ext/lombok.jar"))
  ;;                      ;; "-Xmx8G"
  ;;                      ;; "-XX:+UseG1GC"
  ;;                      ;; "-XX:+UseZGC"
  ;;                      ;; "-XX:+UseStringDeduplication"
  ;;                      ;; "-XX:FreqInlineSize=325"
  ;;                      ;; "-XX:MaxInlineLevel=9"
  ;;                      ;; "-XX:+UseCompressedOops"
  ;;                      ))
  ;;          (jvm-args (mapcar (lambda (arg) (concat "--jvm-arg=" arg)) jvm-args))
  ;;          ;; tell jdtls the data directory and jvm args
  ;;          (contact (append '("jdtls") jvm-args `("-data" ,data-dir))))
  ;;     contact))

  ;; (defun jdtls-initialization-options ()
  ;;   (let ((setting-json-file (file-name-concat user-emacs-directory ".." ".emacs.conf" "lsp-config" "jdtls.json")))
  ;;     (with-temp-buffer
  ;;       (insert-file-contents setting-json-file)
  ;;       (json-parse-buffer :object-type 'plist :false-object :json-false))))

  ;; (cl-defmethod eglot-initialization-options (server &context (major-mode java-mode))
  ;;   (jdtls-initialization-options))

  ;; (cl-defmethod eglot-initialization-options (server &context (major-mode java-ts-mode))
  ;;   (jdtls-initialization-options))

  ;; (add-to-list 'eglot-server-programs '((java-mode java-ts-mode) . jdtls-command-contact))
  )

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; auto-completion and code snippets

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
  (python-mode . (lambda () (setq flycheck-local-checkers '((eglot . ((next-checkers . (python-flake8 python-pylint))))))))
  )

;; (use-package dap-mode
;;   :commands dap-mode
;;   :config
;;   (dap-mode 1)
;;   (require 'dap-ui)
;;   (dap-ui-mode 1)
;;   (require 'dap-lldb))

;; (use-package dap-java :straight nil)

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
  (setq gofmt-command "goimports"))

;; (use-package yasnippet
;;   :config
;;   (yas-reload-all)
;;   (add-hook 'prog-mode-hook 'yas-minor-mode)
;;   (add-hook 'text-mode-hook 'yas-minor-mode))

;; (defun company-yasnippet-or-completion ()
;;   (interactive)
;;   (or (do-yas-expand)
;;       (company-complete-common)))

;; (defun check-expansion ()
;;   (save-excursion
;;     (if (looking-at "\\_>") t
;;       (backward-char 1)
;;       (if (looking-at "\\.") t
;;         (backward-char 1)
;;         (if (looking-at "::") t nil)))))

;; (defun do-yas-expand ()
;;   (let ((yas/fallback-behavior 'return-nil))
;;     (yas/expand)))

;; (defun tab-indent-or-complete ()
;;   (interactive)
;;   (if (minibufferp)
;;       (minibuffer-complete)
;;     (if (or (not yas/minor-mode)
;;             (null (do-yas-expand)))
;;         (if (check-expansion)
;;             (company-complete-common)
;;           (indent-for-tab-command)))))

(use-package company
  :after eglot
  :hook ((after-init . global-company-mode))
  :init
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1)
  :bind
  (:map company-active-map
        ("C-n". company-select-next)
        ("C-p". company-select-previous)
        ("M-<". company-select-first)
        ("M->". company-select-last))
  :config

  ;; (:map company-mode-map
  ;;       ("<tab>". tab-indent-or-complete)
  ;;       ("TAB". tab-indent-or-complete))
  )


(use-package helm
  :custom
  (helm-completion-style 'helm-fuzzy)
  :config
  (helm-mode 1)
  (setq helm-M-x-fuzzy-match t)
  ;; Remapped bindings
  (global-set-key [remap execute-extended-command] 'helm-M-x)
  (global-set-key [remap find-file] 'helm-find-files)
  (global-set-key [remap list-buffers] 'helm-buffers-list)
  (global-set-key [remap yank-pop] 'helm-show-kill-ring)
  (global-set-key (kbd "C-c s") 'helm-semantic-or-imenu))

;; (use-package helm-lsp
;;   :config
;;   (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
;;   (define-key lsp-mode-map [remap lsp-execute-code-action] #'helm-lsp-code-actions))

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

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets :ensure t :after yasnippet)

(use-package treemacs
  :config
  (global-set-key (kbd "C-c t") 'treemacs)
  (treemacs-git-mode 'extended)
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))

(use-package treemacs-projectile :after (treemacs))
;; (use-package lsp-treemacs :after (lsp-mode treemacs)
;;   :config
;;   (lsp-treemacs-sync-mode 1))

(setq compilation-scroll-output t)

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

;; (use-package copilot
;;   :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;;   :hook (prog-mode . copilot-mode)
;;   :bind
;;   (:map copilot-mode-map
;;         ("C-c SPC" . copilot-complete)
;;         :map copilot-completion-map
;;         ("C-n" . copilot-next-completion)
;;         ("C-p" . copilot-previous-completion)
;;         ("<tab>" . copilot-accept-completion)
;;         ("TAB" . copilot-accept-completion))
;;   :config
;;   (setq copilot-disable-predicates '(copilot--buffer-changed)))


(defun kill-to-clipboard ()
  "Use ANSI OSC 52 escape sequence to attempt clipboard copy"
  ;; Modified from https://sunaku.github.io/tmux-yank-osc52.html
  (interactive)
  (let ((tmx_tty (shell-command-to-string "tmux list-panes -F '#{pane_active} #{pane_tty}' | awk '$1==\"1\" { print $2 }'"))
        (base64_text (base64-encode-string (encode-coding-string (substring-no-properties (nth 0 kill-ring)) 'utf-8) t)))
    ;; Check if inside TMUX
    (if (getenv "TMUX")
        (shell-command
         (format "printf \"\033]52;c;%s\a\" > %s" base64_text tmx_tty))
      ;; Check if inside SSH
      (if (getenv "SSH_TTY")
          (shell-command (format "printf \"\033]52;c;%s\a\" > %s" base64_text (getenv "SSH_TTY")))
        ;; Send to current TTY
        (send-string-to-terminal (format "\033]52;c;%s\a" base64_text))))))

(defun after-kill-region-advice (beg end &rest args)
  (kill-to-clipboard))

(defun after-kill-line-advice (&rest args)
  (kill-to-clipboard))

(advice-add 'kill-ring-save :after #'after-kill-region-advice)
(advice-add 'kill-region :after #'after-kill-region-advice)
(advice-add 'kill-line :after #'after-kill-line-advice)


(defun switch-to-buffer-temporarily (&optional buffer)
  "Switch to BUFFER temporarily as the only buffer in the current window.
If BUFFER is not provided, defaults to the current buffer."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))
  ;; Define a display buffer action that makes the buffer occupy the whole frame
  (let ((display-buffer-alist `((,buffer . ((display-buffer-reuse-window
                                             display-buffer-in-side-window)
                                            (inhibit-same-window . t)
                                            (same-window-regex . "*"))))))
    (switch-to-buffer buffer)))
