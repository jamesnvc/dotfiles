;;; -*- lexical-binding: t -*-

(use-package general
  :demand t
  :config
  (setq general-default-keymaps '(evil-normal-state-map evil-visual-state-map))
  ;; (setq general-default-prefix "SPC")
  (general-evil-setup))

(provide 'cogent-keys)
