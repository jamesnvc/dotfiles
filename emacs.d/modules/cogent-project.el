;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package dumb-jump
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :custom ((dumb-jump-selector 'completing-read)))

(use-package project
  :straight (:type built-in)
  :config
  (customize-set-variable
   'vc-directory-exclusion-list
   (cons ".ccls-cache" vc-directory-exclusion-list))
  (cogent/leader-def
    :states '(normal visual)
    "P" #'project-switch-project
    "t" #'project-find-file))

(provide 'cogent-project)
