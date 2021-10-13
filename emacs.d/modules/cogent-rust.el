;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package rust-mode)

(use-package flycheck-rust
  :hook (flycheck-mode-hook . flycheck-rust-setup))

(use-package racer
  :hook ((rust-mode-hook  . racer-mode)
         (racer-mode-hook . eldoc-mode)))

(use-package cargo
  :hook (rust-mode-hook . cargo-minor-mode)
  :config
  (setq compilation-ask-about-save nil))

(provide 'cogent-rust)
