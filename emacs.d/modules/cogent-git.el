;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package magit
  :commands magit-status
  :init
  (defadvice magit-status (after cogent/magit-status-evil-emacs)
    (evil-emacs-state))
  (ad-activate #'magit-status)
  :general
  (general-nvmap :prefix "SPC g"
    "s" #'magit-status
    "w" #'magit-stage-file
    "c" #'magit-commit-create
    "H" #'magit-log-buffer-file))

(use-package ghub
  :straight (ghub
             :type git
             :host github
             :repo "magit/ghub"))

(use-package forge
  :straight (forge
             :type git
             :host github
             :repo "magit/forge")
  :general
  (:states 'normal :keymaps 'magit-mode-map
    "`" #'forge-dispatch))

(use-package evil-magit
  :after magit
  :demand t)

(use-package gist)

(use-package fringe-helper)
(use-package git-gutter-fringe+
  :demand t
  :config
  (global-git-gutter+-mode t)
  :diminish git-gutter-mode
  :general
  (:keymaps 'normal
   :jump t
   "]c" #'git-gutter+-next-hunk
   "[c" #'git-gutter+-previous-hunk)
  (:states '(normal visual) :prefix "SPC h"
    "s" #'git-gutter+-stage-hunks))

(provide 'cogent-git)
