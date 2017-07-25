;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status)
  :config
  (evil-leader/set-key "g s" 'magit-status)
  (use-package evil-magit
    :demand t))

(use-package gist)

(use-package fringe-helper)
(use-package git-gutter-fringe+
  ;; :config
  ;; (global-git-gutter-mode t)
  :diminish git-gutter-mode)

(provide 'cogent-git)
