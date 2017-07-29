;;; -*- lexical-binding: t -*-

(require 'cl-lib)

; Use MELPA repo as well as standard
(setq package-user-dir (concat dotfiles-dir "elpa"))
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

; Paradox is a better interface for package managment
(when (not (package-installed-p 'paradox))
  (package-install 'paradox))

; Going to use 'use-package' to manage depedencies
; This lets us defer loading stuff until needed
; and keeps the configuration together

; Make sure it's installed
(paradox-require 'use-package)
; Load it so it's available
(require 'use-package)

; make use-package install packages as needed, instead of us having to manually do that
(setq use-package-always-ensure t)

(provide 'cogent-package)
