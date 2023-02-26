;;; -*- lexical-binding: t -*-
(require 'cogent-package)

(use-package swift-mode
  :defer t)

(use-package haskell-mode
  :defer t)

(use-package lua-mode
  :defer t
  :mode ("\\.lua\\'" . lua-mode)
  :commands lua-mode)

(use-package fennel-mode
  :defer t
  :mode ("\\.fnl\\'" . fennel-mode)
  :hook (fennel-mode-hook . paredit-mode)
  :commands fennel-mode)

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

(use-package ess)

(use-package go-mode)

(provide 'cogent-misc-langs)
