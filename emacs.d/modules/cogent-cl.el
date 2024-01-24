;;; cogent-cl.el --- Common Lisp config              -*- lexical-binding: t; -*-

(use-package sly
  :config
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)

  (setopt inferior-lisp-program (executable-find "sbcl"))
  (setopt sly-symbol-completion-mode nil)

  (evil-define-operator cogent/evil-sly-eval (beg end)
    "Evaluate Common Lisp expression given by <motion> via sly."
    (sly-eval-region beg end))
  (evil-define-operator cogent/evil-sly-eval-in-place (beg end)
    "Evaluate Common Lisp expression given by <motion> via sly and
insert the result."
    (sly-eval-async `(slynk:eval-and-grab-output ,(buffer-substring-no-properties beg end))
      (lambda (result)
        (cl-destructuring-bind (output value) result
          (push-mark)
          (let* ((start end)
                 (ppss (syntax-ppss))
                 (string-or-comment-p (or (nth 3 ppss) (nth 4 ppss))))
            (save-excursion
              (goto-char end)
              (insert output (if string-or-comment-p
                                 ""
                               " => ") value)
              (unless string-or-comment-p
                (comment-region start (point) 1))))))))
  (general-nmap 'sly-mode-map
    "go" 'cogent/evil-sly-eval
    "g!" 'cogent/evil-sly-eval-in-place
    "M-." 'sly-edit-definition))


(provide 'cogent-cl)
;;; cogent-cl.el ends here
