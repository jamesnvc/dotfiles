;;; -*- lexical-binding: t -*-
(require 'cogent-package)

(use-package clojure-mode
  :commands clojure-mode
  :config
  (add-hook
   'clojure-mode-hook
   (lambda ()
     (paredit-mode)
     (clj-refactor-mode 1)
     ; start an nREPL elsewhere, use M-x monroe to connect
     (use-package monroe
       :commands monroe
       :config
       (clojure-enable-monroe)
       :bind (:map clojure-mode-map ("C-x C-e" . monroe-eval-expression-at-point))
       :init
       (evil-define-key 'normal clojure-mode-map "c p p" 'monroe-eval-expression-at-point)))))

(provide 'cogent-clojure)
