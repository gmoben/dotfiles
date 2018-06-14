(use-package ob-restclient :ensure t)
(use-package ob-rust :ensure t)
(use-package ob-go :ensure t)

(require 'org)
(require 'ob-python)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
     (python . t)
     (sql . t)
     (haskell . t)
     (dot . t)
     (rust . t)
     (restclient . t)
     (shell . t)
     (go . t)
     ))

(setq org-confirm-babel-evaluate t)
