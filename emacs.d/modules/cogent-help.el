;; -*- lexical-binding: t -*-

(require 'cogent-package)

;; Wait two seconds when chording to see available options
(use-package which-key
  :commands which-key-mode
  :demand t
  :config
  (which-key-mode)
  (setq-default which-key-idle-deplay 2.0)
  (setq-default which-key-special-keys nil)
  :bind ("<f1> C-k" . which-key-show-top-level)
  :diminish which-key-mode)

(use-package discover-my-major
  :commands (discover-my-major discover-my-mode)
  :bind ("<f1> C-m" . discover-my-major))

(provide 'cogent-help)
