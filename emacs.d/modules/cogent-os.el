;;; -*- lexical-binding: t -*-

(use-package helm-system-packages
  :commands helm-system-packages)

(use-package disk-usage
  :commands (disk-usage disk-usage-here)
  :config
  (evil-set-initial-state 'disk-usage-mode 'emacs)
  :straight
  (:type git :host github :repo "emacsmirror/disk-usage"))

(provide 'cogent-os)
