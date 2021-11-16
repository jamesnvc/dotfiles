;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package corfu
  :init
  (corfu-global-mode)
  :custom
  (corfu-quit-no-match t)
  :config
  (with-eval-after-load 'lsp
    (setq lsp-completion-provider :none)
    (defun cogent/corfu-lsp-setup ()
      (setq-local completion-styles '(orderless)
                  completion-category-defaults nil))
    (add-hook 'lsp-mode-hook #'cogent/corfu-lsp-setup))
  :general
  (:keymaps 'corfu-map :states 'insert
            [escape] #'corfu-quit)
  (:states 'insert
           "C-\\" #'completion-at-point))

(provide 'cogent-complete)
