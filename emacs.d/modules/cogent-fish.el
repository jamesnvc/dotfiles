;;; -*- lexical-binding: t -*-
(require 'cogent-package)

(use-package fish-mode)

(use-package fish-completion
  :straight (emacs-fish-completion
             :type git
             :host gitlab
             :repo "ambrevar/emacs-fish-completion")
  :commands (global-fish-completion-mode fish-completion-mode)
  :hook (eshell-mode . (lambda () (fish-completion-mode 1))))

(provide 'cogent-fish)
