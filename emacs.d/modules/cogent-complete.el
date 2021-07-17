;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package corfu
  :init
  (corfu-global-mode)
  :custom
  (corfu-quit-no-match t)
  :general
  (:keymaps 'corfu-map :states 'insert
            [escape] #'corfu-quit)
  (:states 'insert
           "C-\\" #'completion-at-point))

(provide 'cogent-complete)
