;;; -*- lexical-binding: t -*-
(require 'cogent-package)

(use-package clojure-mode
  :commands clojure-mode
  :config
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)

  (use-package cider
    :commands cider-jack-in
    :config
    (add-hook 'cider-mode-hook 'eldoc-mode)
    (evil-set-initial-state 'cider-docview-mode 'insert)
    (evil-set-initial-state 'cider-stacktrace-mode 'insert)
    (setq cider-prompt-for-symbol nil)
    (setq cider-font-lock-dynamically '(macro core function var))
    (with-eval-after-load "company"
      (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
      (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))
    (with-eval-after-load "helm"
      (use-package helm-cider
        :config (helm-cider-mode 1))))

  (use-package clj-refactor
    :config
    (add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1))))

  (use-package cljr-helm
    :general
    (:keymaps 'clojure-mode-map :states '(normal)
              "<SPC>r" 'cljr-helm)))

(use-package paredit
  :commands enable-paredit-mode paredit-mode
  :diminish paredit-mode)

(provide 'cogent-clojure)
