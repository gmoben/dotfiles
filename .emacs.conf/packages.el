(package-initialize)

(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(defun ensure-package-installed (packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (package-install package)))
   packages))

(unless package-archive-contents
  (package-refresh-contents))

(when (not (package-installed-p `use-package))
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(unless package-archive-contents
  (package-refresh-contents))

;; (ensure-package-installed '(
;;                             apel
;;                             async
;;                             auto-complete
;;                             badwolf-theme
;;                             bliss-theme
;;                             boron-theme
;;                             buffer-move
;;                             cargo
;;                             coffee-mode
;;                             colonoscopy-theme
;;                             company
;;                             company-go
;;                             company-jedi
;;                             company-lsp
;;                             company-shell
;;                             concurrent
;;                             ctable
;;                             cython-mode
;;                             danneskjold-theme
;;                             dash
;;                             dash-at-point
;;                             deferred
;; ;;                          dired+
;;                             dired-hacks-utils
;;                             dired-single
;;                             dired-subtree
;;                             diredful
;;                             dna-mode
;;                             dockerfile-mode
;;                             emms
;;                             encourage-mode
;;                             epc
;;                             epl
;;                             exec-path-from-shell
;;                             feature-mode
;;                             firecode-theme
;;                             flim
;;                             flx
;;                             flycheck
;;                             flycheck-flow
;;                             flycheck-gometalinter
;;                             flycheck-rust
;;                             format-sql
;;                             ggtags
;;                             gh
;;                             git-commit
;;                             gntp
;;                             go-mode
;;                             google
;;                             gotham-theme
;;                             grizzl
;;                             groovy-imports
;;                             groovy-mode
;;                             haskell-mode
;;                             helm
;;                             helm-ag
;;                             helm-company
;;                             helm-core
;;                             helm-cscope
;;                             helm-descbinds
;;                             helm-describe-modes
;;                             helm-dictionary
;;                             helm-flx
;;                             helm-fuzzier
;;                             helm-google
;;                             helm-gtags
;;                             helm-make
;;                             helm-mode-manager
;;                             helm-org-rifle
;;                             helm-proc
;;                             helm-projectile
;;                             helm-pydoc
;;                             helm-spotify
;;                             helm-swoop
;;                             hemisu-theme
;;                             highlight
;; ;;                          highlight-tail
;;                             ht
;;                             hydra
;;                             import-js
;;                             isend-mode
;;                             jedi
;;                             jenkins-watch
;;                             json-mode
;;                             json-reformat
;;                             json-snatcher
;;                             let-alist
;;                             list-utils
;;                             load-relative
;;                             loc-changes
;;                             log4e
;;                             logito
;;                             lsp-mode
;;                             marshal
;;                             multi
;;                             nhexl-mode
;;                             paradox
;;                             pcache
;;                             pkg-info
;;                             popup
;;                             powerline
;;                             projectile
;;                             protobuf-mode
;;                             py-autopep8
;;                             py-isort
;;                             py-yapf
;;                             pydoc
;;                             pydoc-info
;;                             pytest
;;                             python-docstring
;;                             python-environment
;;                             python-mode
;;                             realgud
;;                             request
;;                             rjsx-mode
;;                             rotate
;;                             rust-mode
;;                             s
;;                             semi
;;                             seq
;;                             speed-type
;;                             sphinx-doc
;;                             spinner
;;                             telephone-line
;;                             test-simple
;;                             use-package
;;                             vagrant
;;                             visual-regexp
;;                             wanderlust
;;                             warm-night-theme
;;                             web-beautify
;;                             with-editor
;;                             xcscope
;;                             xterm-color
;;                             yaml-mode
;;                             yasnippet)) ;  --> (nil nil) if iedit and magit are already installed
