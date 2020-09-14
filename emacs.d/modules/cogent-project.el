;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package projectile
  :commands projectile-mode
  :defer 1
  :bind ("C-c C-f" . projectile-find-file)
  :diminish projectile-mode)

(use-package ibuffer-projectile
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-hook . (lambda ()
                          (ibuffer-projectile-set-filter-groups)
                          (unless (eq ibuffer-sorting-mode 'alphabetic)
                            (ibuffer-do-sort-by-alphabetic)))))

(use-package dumb-jump
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :custom ((dumb-jump-selector 'helm)))


(provide 'cogent-project)
