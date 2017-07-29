;;; -*- lexical-binding: t -*-
;; My emacs configuration, based on ohai-emacs by Bodil Stokke
;; Basically the same, but doing it myself so I understand how things work

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
(require 'cogent-appearance)
(require 'cogent-codestyle)
(require 'cogent-helm)
(require 'cogent-elisp)
(require 'cogent-evil)
(require 'cogent-sexp)
(require 'cogent-complete)
(require 'cogent-editing)
(require 'cogent-project)
(require 'cogent-git)
(require 'cogent-clojure)
(require 'cogent-elixir)
(require 'cogent-html)

;; Other things
(load (concat dotfiles-dir "user.el") 'noerror)
