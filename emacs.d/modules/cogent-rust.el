;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package rust-mode)

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package racer
  :hook ((rust-mode  . racer-mode)
         (racer-mode . eldoc-mode)
         (racer-mode . company-mode))
  :diminish racer-mode)

(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :config
  (setq compilation-ask-about-save nil)
  :diminish cargo-minor-mode)

(provide 'cogent-rust)
