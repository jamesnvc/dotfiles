;;; -*- lexical-binding: t -*-
(require 'cogent-package)

(use-package clojure-mode
  :commands clojure-mode
  :config
  (add-hook
   'clojure-mode-hook
   (lambda ()
     (paredit-mode)
     ;; start an nREPL elsewhere, use M-x monroe to connect
     (use-package cider
       :commands cider-jack-in)))
  ;; Add completion using monroe + company-mode
  (with-eval-after-load "company"
    (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)))

(use-package paredit
  :commands enable-paredit-mode paredit-mode
  :diminish paredit-mode
  :init
  (add-hook 'clojure-mode-hook 'enable-paredit-mode))

(provide 'cogent-clojure)
