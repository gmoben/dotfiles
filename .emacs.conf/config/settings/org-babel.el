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

(defun org-babel-execute:org (body params)
  "Return BODY with variables from PARAMS replaced by their values."
  (let* ((vars (cl-loop for par in params
            if (eq (car par) :var)
            collect (cons (symbol-name (cadr par)) (cddr par))))
     (re (regexp-opt (mapcar #'car vars) 'words))
     (pos 0))
    (while (string-match re body pos)
      (setq body (replace-match
          (format "%s" (cdr (assoc-string (match-string 0 body) vars)))
          nil nil
          body)))
    body))
