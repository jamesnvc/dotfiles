;;; -*- lexical-binding: t -*-
(require 'cogent-package)

(use-package fish-mode)

(use-package fish-completion
  :elpaca (emacs-fish-completion
           :type git
           :host gitlab
           :repo "ambrevar/emacs-fish-completion")
  :commands (global-fish-completion-mode fish-completion-mode)
  :hook (eshell-mode-hook . fish-completion-mode))

(provide 'cogent-fish)
