;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package transient
  ;; transient now part of Emacs!
  :straight (:type built-in))

(use-package magit
  :commands magit-status
  :init
  (evil-set-initial-state 'git-commit-mode evil-default-state)
  :general
  (general-nvmap :prefix "SPC g"
    "s" #'magit-status
    "w" #'magit-stage-file
    "c" #'magit-commit-create
    "H" #'magit-log-buffer-file))

(use-package ghub
  :after magit
  :straight (ghub
             :type git
             :host github
             :repo "magit/ghub"))

(use-package forge
  :after magit
  :straight (forge
             :type git
             :host github
             :repo "magit/forge")
  :config
  (evil-set-initial-state 'forge-topic-mode 'emacs))

(use-package diff-hl
  :config
  (diff-hl-flydiff-mode -1)
  :custom
  ((diff-hl-draw-borders nil))
  :hook
  ((after-init-hook . global-diff-hl-mode)
   (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
   (magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :general
  (:keymaps 'normal :jump t
            "[c" #'diff-hl-previous-hunk
            "]c" #'diff-hl-next-hunk))

(use-package vc-annotate
  :straight (:type built-in)
  :custom
  ((vc-annotate-display-mode 'scale)))

(provide 'cogent-git)
