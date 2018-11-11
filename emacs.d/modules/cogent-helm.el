;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package helm
  :config
  (require 'helm-config)
  (require 'helm)
  (helm-mode 1)
  (with-eval-after-load "cogent-project"
    (use-package helm-projectile
      :commands helm-projectile-on helm-projectile-find-file
      :config (projectile-mode)))
  (helm-autoresize-mode 1)
  (setq-default helm-display-header-line nil
                helm-autoresize-min-height 0
                helm-autoresize-max-height 35
                helm-split-window-inside-p t

                helm-M-x-fuzzy-match t
                helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-match t
                helm-apropos-fuzzy-match t)
  (set-face-attribute 'helm-source-header nil :height 0.75)
  (diminish 'helm-mode)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-g" . helm-do-grep)
         ("C-x b" . helm-buffers-list)
         ("C-t" . helm-imenu)
         ("M-y" . helm-show-kill-ring)))

(use-package swiper-helm
  :bind (("C-S-s" . swiper-helm)))

(use-package helm-flx
  :config
  (with-eval-after-load "helm"
    (require 'helm-flx)
    (helm-flx-mode 1)))

(use-package helm-ag)

(use-package helm-ext
  :config
  (helm-ext-ff-enable-skipping-dots t)
  (helm-ext-ff-enable-auto-path-expansion t))

(provide 'cogent-helm)
