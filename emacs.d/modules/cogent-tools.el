;;; -*- lexical-binding: t -*-

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

(use-package tb-keycast
  :commands tb-keycast-mode
  :straight (tb-keycast
             :type git
             :host github
             :repo "ir33k/tb-keycast"))

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

;; (use-package dungeon-mode
;;   :straight (dungeon-mode
;;              :repo "https://git.savannah.nongnu.org/git/dungeon.git"))

(use-package chess)

(use-package sketch-mode
  :defer t
  :commands sketch
  :straight (sketch-mode
             :type git
             :host github
             :repo "dalanicolai/sketch-mode"))

(provide 'cogent-tools)
