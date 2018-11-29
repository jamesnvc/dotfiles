;;; -*- lexical-binding: t -*-
(require 'cogent-package)

(use-package fish-mode)

(use-package fish-completion
  :straight (emacs-fish-completion
             :type git
             :host gitlab
             :repo "ambrevar/emacs-fish-completion")
  :init
  (when (and (cogent/is-exec "fish")
             (require 'fish-completion nil t))
    (global-fish-completion-mode)))

(provide 'cogent-fish)
