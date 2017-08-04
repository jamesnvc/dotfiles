;;; -*- lexical-binding: t -*-

(require 'cogent-package)
(require 'cogent-editing)

;; Special mode for HTML that deals with embedded JS & CSS, JSX, and
;; various templating systems
;; See: http://web-mode.org/
(use-package web-mode
  :mode (;; Use web-mode for HTML instead of default html-mode
         ("\\.html?\\'" . web-mode)
         ("\\.mustache\\'" . web-mode))
  :config
  ;; Highlight the element under the cursor
  (setq-default web-mode-enable-current-element-highlight t))

(use-package rainbow-mode
  :config
  (dolist (mode '(css-mode less-css-mode html-mode web-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              (lambda () (rainbow-mode))))
  :diminish rainbow-mode)

(provide 'cogent-html)
