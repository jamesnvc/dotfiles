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
  :hook ((css-mode-hook . rainbow-mode)
         (less-css-mode-hook . rainbow-mode)
         (html-mode-hook . rainbow-mode)
         (web-mode-hook . rainbow-mode))
  :custom ((rainbow-html-colors t)))

(provide 'cogent-html)
