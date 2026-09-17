;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package reader
  :straight '(reader :type git
                     :host codeberg
                     :repo "divyaranjan/emacs-reader"
                     :files ("*.el" "render-core.dylib")
                     :pre-build ("make" "all"))
  :config
  (add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . reader-mode))
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . reader-mode))
  (add-to-list 'magic-mode-alist '("%PDF" . reader-mode))
  (evil-set-initial-state 'reader-mode 'emacs)
  (add-hook 'reader-mode-hook (lambda () (display-line-numbers-mode -1))))

(provide 'cogent-reading)
