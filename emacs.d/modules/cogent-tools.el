;;; -*- lexical-binding: t -*-

(use-package helm-system-packages
  :commands helm-system-packages)

(use-package disk-usage
  :commands (disk-usage disk-usage-here)
  :config
  (evil-set-initial-state 'disk-usage-mode 'emacs)
  :straight
  (:type git :host github :repo "emacsmirror/disk-usage"))

(use-package pass
  :commands pass
  :config
  (evil-set-initial-state 'pass-mode 'emacs)
  (setq-default password-store-password-length 18))

(use-package keycast
  :commands keycast-mode
  :straight (keycast
             :type git
             :host github
             :repo "tarsius/keycast")
  :config
  (setq keycast-insert-after '(:eval (list (nyan-create)))))

(use-package gif-screencast
  :commands gif-screencast
  :straight (gif-screencast
             :type git
             :host gitlab
             :repo "ambrevar/emacs-gif-screencast")
  :config
  (when (string= system-type "darwin")
    (setq gif-screencast-args '("-x")))
  (setq gif-screencast-cropping-program "mogrify"))

(use-package direnv
  :config (direnv-mode))

(use-package nix-mode
  :mode "\\.nix\\'")

(provide 'cogent-tools)
