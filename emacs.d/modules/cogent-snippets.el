;;; -*- lexical-binding: t -*-

(use-package yasnippet
  :demand t
  :config
  (yas-global-mode 1)
  (with-eval-after-load 'company
    (add-to-list 'company-backends #'company-yasnippet t)))

(provide 'cogent-snippets)
