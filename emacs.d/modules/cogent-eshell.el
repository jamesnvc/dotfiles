;;; -*- lexical-binding: t -*-

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(add-hook 'eshell-mode-hook
          (lambda () (define-key eshell-mode-map (kbd "M-r")
                  #'helm-eshell-history)))

(provide 'cogent-eshell)
