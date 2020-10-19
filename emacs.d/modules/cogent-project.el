;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package projectile
  :commands projectile-mode
  :defer 1
  :bind ("C-c C-f" . projectile-find-file))

(use-package dumb-jump
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :custom ((dumb-jump-selector 'helm)))


(provide 'cogent-project)
