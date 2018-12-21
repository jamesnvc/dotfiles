;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package magit
  :commands magit-status
  :general
  (general-nvmap :prefix "SPC g"
    "s" #'magit-status
    "w" #'magit-stage-file
    "c" #'magit-commit
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
  (general-nmap :keymaps 'magit-mode-map
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
  (general-nvmap :prefix "SPC h"
    "s" #'git-gutter+-stage-hunks))

(provide 'cogent-git)
