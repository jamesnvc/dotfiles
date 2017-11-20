;;; -*- lexical-binding: t -*-
(require 'cogent-package)

(defun cogent/evil-slime-edit-definition (&optional name where)
  )

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
  ;(add-function :before 'slime-edit-definition #'(lambda (&rest args) (evil-set-jump)))
  (evil-define-key 'normal slime-mode-map (kbd "] C-d") #'slime-edit-definition))

(provide 'cogent-common-lisp)
