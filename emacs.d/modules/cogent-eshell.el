;;; -*- lexical-binding: t -*-

(global-set-key (kbd "<f3>") 'eshell)

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(provide 'cogent-eshell)
