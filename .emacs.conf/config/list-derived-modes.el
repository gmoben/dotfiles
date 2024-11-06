(defun list-derived-modes ()
  "Display a hierarchy of all modes derived from `chosen-mode`."
  (interactive)
  (let* ((mode-symbols (let ((acc '()))
                         (mapatoms (lambda (symbol)
                                     (let ((name (symbol-name symbol)))
                                       (when (string-suffix-p "mode" name)
                                         (push name acc)))))
                         acc))
         (chosen-mode-name (completing-read "Select a mode: " mode-symbols))
         (chosen-mode (intern chosen-mode-name)))

    (defun collect-derived-modes (mode &optional level)
      "Recursively collect modes derived from MODE."
      (unless level (setq level 0))
      (let ((derived-modes ()))
        (mapatoms
         (lambda (symbol)
           (when (and (symbol-function symbol)
                      (symbolp (get symbol 'derived-mode-parent)))
             (let ((parent (get symbol 'derived-mode-parent)))
               (when (eq parent mode)
                 (push (cons symbol (collect-derived-modes symbol (1+ level))) derived-modes))))))
        (sort derived-modes (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b)))))))

    (defun print-mode-hierarchy (modes &optional buffer indent)
      "Print a hierarchy of MODES to BUFFER with INDENT."
      (unless buffer (setq buffer (get-buffer-create "*Mode Hierarchy*")))
      (unless indent (setq indent ""))
      (with-current-buffer buffer
        (dolist (mode modes)
          (insert (format "%s%s\n" indent (car mode)))
          (print-mode-hierarchy (cdr mode) buffer (concat indent "  "))))
      (display-buffer buffer))

    ;; Start the hierarchy with the chosen-mode itself
    (with-current-buffer (get-buffer-create "*Mode Hierarchy*")
      (erase-buffer)
      (insert (format "%s\n" chosen-mode-name)) ; Insert the chosen mode at the top
      (let ((derived-modes (collect-derived-modes chosen-mode)))
        (print-mode-hierarchy derived-modes nil "  "))))) ; Indent derived modes
