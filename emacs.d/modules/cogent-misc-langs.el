;;; -*- lexical-binding: t -*-
(require 'cogent-package)

(use-package swift-mode
  :defer t)

(use-package haskell-mode
  :defer t)

(use-package lua-mode
  :defer t)

;;(use-package racket-mode)

(use-package yaml-mode
  :defer t)

(use-package scala-mode
  :defer t
  :mode ("\\.scala\\'" . scala-mode)
  :commands scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package j-mode
  :defer t
  :straight (j-mode
             :type git
             :host github
             :repo "zellio/j-mode")
  :config
  (setq j-console-cmd "ijconsole"))

(provide 'cogent-misc-langs)
