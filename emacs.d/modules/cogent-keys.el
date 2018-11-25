;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package general
  :demand t
  :config
  ;;(setq general-default-keymaps '(evil-normal-state-map evil-visual-state-map))
  ;; (setq general-default-prefix "SPC")
  (general-evil-setup)
  (general-create-definer cogent/leader-def :prefix "SPC")
  (general-auto-unbind-keys))

(provide 'cogent-keys)
