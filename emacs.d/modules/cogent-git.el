;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package transient
  :straight (transient
             :type git
             :host github
             :repo "magit/transient"))

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

(use-package fringe-helper)
(use-package git-gutter-fringe+
  :commands (global-git-gutter+-mode git-gutter+-mode)
  :defer t
  :init
  (defun cogent/load-git-gutter-once ()
    (remove-hook 'find-file-hook #'cogent/load-git-gutter-once)
    (global-git-gutter+-mode t))
  (add-hook 'find-file-hook #'cogent/load-git-gutter-once)
  :diminish git-gutter-mode
  :general
  (:keymaps 'normal
   :jump t
   "]c" #'git-gutter+-next-hunk
   "[c" #'git-gutter+-previous-hunk)
  (:states '(normal visual) :prefix "SPC h"
    "s" #'git-gutter+-stage-hunks))

(provide 'cogent-git)
