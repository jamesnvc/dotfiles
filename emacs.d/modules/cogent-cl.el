;;; cogent-cl.el --- Common Lisp config              -*- lexical-binding: t; -*-

(use-package sly
  :config
  (setopt inferior-lisp-program (executable-find "sbcl"))
  (evil-define-operator cogent/evil-sly-eval (beg end)
    "Evaluate Common Lisp expression given by <motion> via sly."
    (sly-eval-with-transcript `(slynk:interactive-eval
                                ,(buffer-substring-no-properties beg end))))

  (general-nmap 'sly-mode-map
    "go" 'cogent/evil-sly-eval))


(provide 'cogent-cl)
;;; cogent-cl.el ends here
