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
    (setq slime-contribs '(slime-fancy slime-company))))

(provide 'cogent-common-lisp)
