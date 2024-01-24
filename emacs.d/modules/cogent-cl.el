;;; cogent-cl.el --- Common Lisp config              -*- lexical-binding: t; -*-

(use-package sly
  :config
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)

  (setopt inferior-lisp-program (executable-find "sbcl"))
  (setopt sly-symbol-completion-mode nil)

  (defun cogent-lisp/form-in-region (beg end)
    "Get the lisp code to evaluate in region, unescaping strings if necessary."
    (let* ((ppss (syntax-ppss))
           (in-string-p (nth 3 ppss))
           (string (buffer-substring-no-properties beg end)))
      (if in-string-p
          (string-replace "\\" "" string)
        string)))

  (evil-define-operator cogent/evil-sly-eval (beg end)
    "Evaluate Common Lisp expression given by <motion> via sly."
    (sly-eval-with-transcript
     `(slynk:interactive-eval-region ,(cogent-lisp/form-in-region beg end))))

  (evil-define-operator cogent/evil-sly-eval-in-place (beg end)
    "Evaluate Common Lisp expression given by <motion> via sly and
insert the result."
    (sly-eval-async `(slynk:eval-and-grab-output
                      ,(cogent-lisp/form-in-region beg end))
      (lambda (result)
        (cl-destructuring-bind (output value) result
          (let* ((ppss (syntax-ppss))
                 (string-or-comment-p (or (nth 3 ppss) (nth 4 ppss))))
            (save-excursion
              (goto-char end)
              (insert output " => " value)
              (unless string-or-comment-p
                (comment-region end (point) 1))))))))

  (general-nmap 'sly-mode-map
    "go" 'cogent/evil-sly-eval
    "g!" 'cogent/evil-sly-eval-in-place
    "M-." 'sly-edit-definition))


(provide 'cogent-cl)
;;; cogent-cl.el ends here
