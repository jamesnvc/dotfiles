;;; -*- lexical-binding: t -*-
(require 'cogent-package)

(use-package clojure-mode
  :commands clojure-mode
  :hook (clojure-mode . enable-paredit-mode))

(use-package cljr-helm
  :after general clojure-mode
  :general
  (:keymaps 'clojure-mode-map :states '(normal)
            "<SPC>r" 'cljr-helm))

(use-package clj-refactor
  :after clojure-mode
  :hook (clojure-mode . (lambda () (clj-refactor-mode 1))))

(use-package cider
  :commands cider-jack-in
  :hook (cider-mode . eldoc-mode)
  :config
  (evil-set-initial-state 'cider-docview-mode 'insert)
  (evil-set-initial-state 'cider-stacktrace-mode 'insert)
  (setq cider-prompt-for-symbol nil)
  (setq cider-font-lock-dynamically '(macro core function var))
  (with-eval-after-load "company"
    (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)))

(use-package helm-cider
  :after helm cider
  :config (helm-cider-mode 1))

(use-package paredit
  :commands enable-paredit-mode paredit-mode
  :diminish paredit-mode)

;; Like vim-fireplace
(evil-define-operator cogent/evil-cider-eval (beg end)
 "Evaluate clojure expression given by <motion> via cider."
  (cider-eval-region beg end))

(evil-define-operator cogent/evil-cider-eval-replace (beg end)
  "Evaluate clojure expression given by <motion> via cider and replace
the expression with the result."
  (let ((exp (read (buffer-substring-no-properties beg end))))
    (cider-nrepl-sync-request:eval exp)
    (delete-region beg end)
    (cider-interactive-eval exp (cider-eval-print-handler))))

(general-nmap 'cider-mode-map
  "go" 'cogent/evil-cider-eval
  "g!" 'cogent/evil-cider-eval-replace
  ;; prefix arg to debug defun
  "gd" #'cogent/eval-defun-at-point
  "] C-d" #'cider-find-var
  "K" #'cider-doc
  "M-r" (lambda () (interactive) (cider-load-file (buffer-file-name))))

(provide 'cogent-clojure)
