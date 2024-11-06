;; `M-x combobulate' (default: `C-c o o') to start using Combobulate
(use-package treesit
  :ensure nil
  :straight nil
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
              '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                (java . ("https://github.com/tree-sitter/tree-sitter-java" "v0.20.2" "src"))
                (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
                (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                ;; (kotlin . ("https://github.com/fwcd/tree-sitter-kotlin" "0.3.5"))
                (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                (toml "https://github.com/tree-sitter/tree-sitter-toml")
                (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
         '((bash-mode . bash-ts-mode)
           (css-mode . css-ts-mode)
           (css-mode . css-ts-mode)
           (java-mode . java-ts-mode)
           (js-json-mode . json-ts-mode)
           (js2-mode . js-ts-mode)
           (json-mode . json-ts-mode)
           ;; (kotlin-mode . kotlin-ts-mode)
           (python-mode . python-ts-mode)
           (typescript-mode . typescript-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars)
  (add-to-list 'auto-mode-alist '("\\.json\\(\\.?te?mpl\\(ate\\)?\\)?\\'" . json-ts-mode))
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  (use-package combobulate
    :preface
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (setq combobulate-key-prefix "C-c o")

    ;; Optional, but recommended.
    ;;
    ;; You can manually enable Combobulate with `M-x
    ;; combobulate-mode'.
    :hook
    ((css-ts-mode . combobulate-mode)
     (html-ts-mode . combobulate-mode)
     (java-ts-mode . combobulate-mode)
     (js-ts-mode . combobulate-mode)
     (json-ts-mode . combobulate-mode)
     (kotlin-ts-mode . combobulate-mode)
     (python-ts-mode . combobulate-mode)
     (tsx-ts-mode . combobulate-mode)
     (typescript-ts-mode . combobulate-mode)
     (yaml-ts-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
    :load-path ("path-to-git-checkout-of-combobulate")))
