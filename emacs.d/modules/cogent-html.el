;;; -*- lexical-binding: t -*-

(require 'cogent-package)

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
  :hook ((css-mode . rainbow-mode)
         (less-css-mode . rainbow-mode)
         (html-mode . rainbow-mode)
         (web-mode . rainbow-mode))
  :diminish rainbow-mode)

(provide 'cogent-html)
