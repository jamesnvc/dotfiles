;;; -*- lexical-binding: t -*-
;; My emacs configuration, based on ohai-emacs by Bodil Stokke
;; Basically the same, but doing it myself so I understand how things work

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d gcs"
                     (format "%.2f seconds" (float-time (time-subtract
                                                         after-init-time
                                                         before-init-time)))
                     gcs-done)))

(setq gc-cons-threshold 128000000)
(add-hook 'after-init-hook (lambda ()
                             ;; restore after startup
                             (setq gc-cons-threshold (default-value 'gc-cons-threshold))))

(setq inhibit-startup-message t)

(defvar dotfiles-dir
  (file-name-directory (or buffer-file-name
                           (file-chase-links load-file-name)))
  "Directory in which the emacs dotfiles live -- presumably ~/.config/emacs for >28, ~/.emacs.d for older.")

(add-to-list 'load-path (concat dotfiles-dir "modules"))

;; Define where to keep autoload declarations and custom settings
(setq autoload-file (concat dotfiles-dir "loaddefs.el")) ; still need this?
(setq custom-file (concat dotfiles-dir "custom.el"))

(load custom-file 'noerror)

;; Load packages
(require 'cogent-package)
(require 'cogent-general)
(require 'cogent-base)
(require 'cogent-keys)
(require 'cogent-appearance)
(require 'cogent-fonts)
(require 'cogent-windows)
(require 'cogent-evil)
(require 'cogent-reading) ; needs to load before org
(require 'cogent-orgmode)
(require 'cogent-codestyle)
(require 'cogent-shell)
;; (require 'cogent-helm)
(require 'cogent-unhelm)
(require 'cogent-flycheck)
(require 'cogent-elisp)
(require 'cogent-sexp)
(require 'cogent-complete)
(require 'cogent-editing)
(require 'cogent-project)
(require 'cogent-dired)
(require 'cogent-git)
(require 'cogent-clojure)
(require 'cogent-elixir)
(require 'cogent-html)
(require 'cogent-javascript)
(require 'cogent-rust)
(require 'cogent-markdown)
(require 'cogent-prolog)
(require 'cogent-mail)
(require 'cogent-feeds)
(require 'cogent-misc-langs)
(require 'cogent-fish)
(require 'cogent-lsp)
(require 'cogent-help)
(require 'cogent-snippets)
(require 'cogent-tools)
(require 'cogent-writing)
(require 'cogent-irc)

;; Other things
(load (concat dotfiles-dir "user.el") 'noerror)
(put 'list-timers 'disabled nil)
