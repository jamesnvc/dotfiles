;;; -*- lexical-binding: t -*-
(require 'cogent-package)

(use-package slime
  :commands slime
  :config
  (setq inferior-lisp-program
        (if-let (roswell (cogent/resolve-exec "ros"))
            (s-concat roswell " -Q run")
          "sbcl"))
  (use-package slime-company
    :demand t
    :config
    (setq slime-contribs '(slime-fancy slime-company)))
  :init
  (evil-define-key 'normal slime-mode-map (kbd "] C-d") #'slime-edit-definition))

(provide 'cogent-common-lisp)
