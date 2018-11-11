;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package json-mode
  :commands json-mode
  :config
  (bind-keys :map json-mode-map
             ("C-c <tab>" . json-mode-beautify)))

(provide 'cogent-json)
