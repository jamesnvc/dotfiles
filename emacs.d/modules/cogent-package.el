;;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defun online? ()
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (cl-some (lambda (iface) (unless (equal "lo" (car iface))
                         (member 'up (car (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))

; Use MELPA repo as well as standard
(setq package-user-dir (concat dotfiles-dir "elpa"))
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(when (online?)
  (unless package-archive-contents (package-refresh-contents)))

; Paradox is a better interface for package managment
(when (not (package-installed-p 'paradox))
  (package-install 'paradox))

;; Going to use 'use-package' to manage depedencies
;; This lets us defer loading stuff until needed
;; and keeps the configuration together

;; Make sure it's installed
(paradox-require 'use-package)
; Load it so it's available
(require 'use-package)

; make use-package install packages as needed, instead of us having to manually do that
(setq use-package-always-ensure t)

(provide 'cogent-package)
