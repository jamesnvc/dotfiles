;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package dumb-jump
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :custom ((dumb-jump-selector 'completing-read)))

(provide 'cogent-project)
