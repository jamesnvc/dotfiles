;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package markdown-mode
  :commands markdown-mode
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook #'flyspell-mode)
  (add-hook 'markdown-mode-hook #'visual-line-mode))

(provide 'cogent-markdown)
