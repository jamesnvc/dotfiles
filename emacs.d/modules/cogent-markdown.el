;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package markdown-mode
  :commands markdown-mode
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode))
  :hook ((markdown-mode . flyspell-mode)
         (markdown-mode . visual-line-mode)))

(provide 'cogent-markdown)
