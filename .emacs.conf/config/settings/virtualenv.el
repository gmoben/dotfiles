(require 'projectile)

;; https://emacs.stackexchange.com/a/5466
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
