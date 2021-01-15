;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package general
  :demand t
  :config
  (general-evil-setup)
  (general-create-definer cogent/leader-def :prefix "SPC")
  (general-auto-unbind-keys))

(use-package emacs
  :bind
  (("<f1> K" . #'describe-keymap)))

(provide 'cogent-keys)
