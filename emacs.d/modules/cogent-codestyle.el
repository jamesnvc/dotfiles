;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(set-default 'indent-tabs-mode nil)
(setq sentence-end-double-space nil)

(define-key global-map (kbd "RET") #'newline-and-indent)

;; Strict whitespace, highlight problems, automatically clean up on save
;; see https://github.com/glasserc/ethan-wspace
(use-package ethan-wspace
  :demand t
  :commands global-ethan-wspace-mode
  :config
  (global-ethan-wspace-mode 1)
  :bind ("C-c c" . ethan-wspace-clean-all)
  :diminish ethan-wspace-mode)

;; Not sure how I feel about these?
(setq mode-require-final-newline nil)
(setq require-final-newline nil)

(setq-default tab-width 2)
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default py-indent-offset 2)
(setq-default nxml-child-indent 2)
(setq-default c-basic-offset 2)
(setq-default web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-style-padding 2
              web-mode-script-padding 2)

(setq c-default-style
      '((awk-mode . "awk")
        (other . "java")))

(provide 'cogent-codestyle)
