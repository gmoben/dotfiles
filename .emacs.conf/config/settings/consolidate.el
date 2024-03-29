(use-package auto-minor-mode)

(use-package any-ini-mode
  :load-path "../.emacs.conf/lisp"
  :config
  (add-to-list 'auto-mode-alist '(".*\\.ini$" . any-ini-mode))
  (add-to-list 'auto-mode-alist '(".*\\.conf$" . any-ini-mode)))

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

(use-package typescript-mode)

(use-package eglot
  :straight nil
  :init
  :defer
  :hook ((sh-mode . eglot-ensure)
         (ruby-mode . eglot-ensure)
         (rustic-mode . eglot-ensure)
         (java-mode . eglot-ensure)
         (java-ts-mode . eglot-ensure)
         (javascript-mode . eglot-ensure)
         (typescript-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename))
  :config

  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode) . ("jdtls" "--jvm-arg=-javaagent:/code/ext/lombok.jar" ;; This should be the script to run jdtls. You can create it yourself, or use the script provided by jdt (requires python 3.9.)
                                             ;; Alternatively, you can paste the command here (e.g. "java" "-Declipse.application=org.eclipse.jdt.ls.core.id1" ...)
                                             ;; See https://github.com/eclipse/eclipse.jdt.ls for more info.
                                             :initializationOptions (:extendedClientCapabilities (:classFileContentsSupport t )))))

  ;; The jdt server sometimes returns jdt:// scheme for jumping to definition
  ;; instead of returning a file. This is not part of LSP and eglot does not
  ;; handle it. The following code enables eglot to handle jdt files.
  ;; See https://github.com/yveszoundi/eglot-java/issues/6 for more info.
  (defun jdt-file-name-handler (operation &rest args)
    "Support Eclipse jdtls `jdt://' uri scheme."
    (let* ((uri (car args))
           (cache-dir "/tmp/.eglot")
           (source-file
            (directory-abbrev-apply
             (expand-file-name
              (file-name-concat
               cache-dir
               (save-match-data
                 (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri))
                 (message "URI:%s" uri)
                 (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))))))
      (unless (file-readable-p source-file)
        (let ((content (jsonrpc-request (eglot-current-server) :java/classFileContents (list :uri uri)))
              (metadata-file (format "%s.%s.metadata"
                                     (file-name-directory source-file)
                                     (file-name-base source-file))))
          (message "content:%s" content)
          (unless (file-directory-p cache-dir) (make-directory cache-dir t))
          (with-temp-file source-file (insert content))
          (with-temp-file metadata-file (insert uri))))
      source-file))

  (add-to-list 'file-name-handler-alist '("\\`jdt://" . jdt-file-name-handler))

  (defun jdthandler--wrap-legacy-eglot--path-to-uri (original-fn &rest args)
    "Hack until eglot is updated.
ARGS is a list with one element, a file path or potentially a URI.
If path is a jar URI, don't parse. If it is not a jar call ORIGINAL-FN."
    (let ((path (file-truename (car args))))
      (if (equal "jdt" (url-type (url-generic-parse-url path)))
          path
        (apply original-fn args))))

  (defun jdthandler--wrap-legacy-eglot--uri-to-path (original-fn &rest args)
    "Hack until eglot is updated.
ARGS is a list with one element, a URI.
If URI is a jar URI, don't parse and let the `jdthandler--file-name-handler'
handle it. If it is not a jar call ORIGINAL-FN."
    (let ((uri (car args)))
      (if (and (stringp uri)
               (string= "jdt" (url-type (url-generic-parse-url uri))))
          uri
        (apply original-fn args))))

  (defun jdthandler-patch-eglot ()
    "Patch old versions of Eglot to work with Jdthandler."
    (interactive) ;; TODO, remove when eglot is updated in melpa
    (unless (and (advice-member-p #'jdthandler--wrap-legacy-eglot--path-to-uri 'eglot--path-to-uri)
                 (advice-member-p #'jdthandler--wrap-legacy-eglot--uri-to-path 'eglot--uri-to-path))
      (advice-add 'eglot--path-to-uri :around #'jdthandler--wrap-legacy-eglot--path-to-uri)
      (advice-add 'eglot--uri-to-path :around #'jdthandler--wrap-legacy-eglot--uri-to-path)
      (message "[jdthandler] Eglot successfully patched.")))

  ;; invoke
  (jdthandler-patch-eglot)

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

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
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
  ;; :hook
  ;; (python-mode . (lambda () (setq flycheck-local-checkers '((lsp . ((next-checkers . (python-flake8 python-pylint))))))))
  )

(use-package dap-mode
  :commands dap-mode
  :config
  (dap-mode 1)
  (require 'dap-ui)
  (dap-ui-mode 1)
  (require 'dap-lldb))

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

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :bind
  (:map copilot-mode-map
        ("C-c SPC" . copilot-complete)
        :map copilot-completion-map
        ("C-n" . copilot-next-completion)
        ("C-p" . copilot-previous-completion)
        ("<tab>" . copilot-accept-completion)
        ("TAB" . copilot-accept-completion))
  :config
  (setq copilot-disable-predicates '(copilot--buffer-changed)))
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
