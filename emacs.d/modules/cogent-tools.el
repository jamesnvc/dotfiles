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
  :defer t
  :config (direnv-mode))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package speed-type
  :defer t)

(use-package explain-pause-mode
  :commands explain-pause-mode
  :straight (explain-pause-mode
             :type git
             :host github
             :repo "lastquestion/explain-pause-mode"))

(use-package org-drill
  :straight (org-drill
             :type git
             :host gitlab
             :repo "phillord/org-drill")
  :commands (org-drill))

(use-package vterm
  :commands (vterm vterm-other-window)
  :custom ((vterm-shell (executable-find "fish")))
  :hook (vterm-mode . (lambda () (display-line-numbers-mode -1))))

(provide 'cogent-tools)
