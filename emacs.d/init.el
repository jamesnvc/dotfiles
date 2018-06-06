;;; -*- lexical-binding: t -*-
;; My emacs configuration, based on ohai-emacs by Bodil Stokke
;; Basically the same, but doing it myself so I understand how things work

(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

(package-initialize)

(setq inhibit-startup-message t)

(setq dotfiles-dir (file-name-directory (or (buffer-file-name) (file-chase-links load-file-name))))

(add-to-list 'load-path (concat dotfiles-dir "modules"))

;; Define where to keep autoload declarations and custom settings
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file (concat dotfiles-dir "custom.el"))

(load custom-file 'noerror)

;; Load packages
(require 'cogent-general)
(require 'cogent-base)
(require 'cogent-package)
(require 'cogent-keys)
(require 'cogent-appearance)
(require 'cogent-fonts)
(require 'cogent-windows)
(require 'cogent-evil)
(require 'cogent-codestyle)
(require 'cogent-eshell)
(require 'cogent-helm)
(require 'cogent-flycheck)
(require 'cogent-elisp)
(require 'cogent-sexp)
(require 'cogent-complete)
(require 'cogent-editing)
(require 'cogent-project)
(require 'cogent-dired)
(require 'cogent-git)
(require 'cogent-clojure)
(require 'cogent-racket)
(require 'cogent-elixir)
(require 'cogent-html)
(require 'cogent-javascript)
(require 'cogent-haskell)
(require 'cogent-rust)
(require 'cogent-lua)
(require 'cogent-orgmode)
(require 'cogent-markdown)
(require 'cogent-prolog)
(require 'cogent-yaml)
(require 'cogent-mail)
(require 'cogent-twitter)
(require 'cogent-feeds)

;; Other things
(load (concat dotfiles-dir "user.el") 'noerror)
