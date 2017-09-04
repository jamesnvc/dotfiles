;;; -*- lexical-binding: t -*-
(require 'cogent-package)

(use-package geiser
  :commands run-geiser
  :init
  (use-package quack
    :demand t)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  (evil-define-key 'normal geiser-mode-map (kbd "] C-d")
    #'geiser-edit-definition))

(provide 'cogent-racket)
