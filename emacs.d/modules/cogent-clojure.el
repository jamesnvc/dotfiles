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

;; Like vim-fireplace
(defun cogent/cider-eval-next-sexp (&optional prefix)
  "Wrap `cider-eval-last-sexp' for evil-mode, by moving one character ahead"
  (interactive "P")
  (save-excursion
    (cogent/evil-forward-sexp)
    (forward-char)
    (cider-eval-last-sexp prefix)))

(defun cogent/cider-eval-next-sexp-and-replace ()
  "Wrap `cider-eval-last-sexp-and-replace' for evil-mode, by moving one character ahead"
  (interactive)
  (save-excursion
    (cogent/evil-forward-sexp)
    (forward-char)
    (cider-eval-last-sexp-and-replace)))

(general-nmap :keymaps 'cider-mode-map
              "c" (general-key-dispatch 'evil-change
                    "pp" 'cogent/cider-eval-next-sexp
                    "p!" 'cogent/cider-eval-next-sexp-and-replace
                    "c" 'evil-change-whole-line)
              "] C-d" 'cider-find-var
              "K" 'cider-doc
              "M-r" #'(lambda () (interactive) (cider-load-file (buffer-file-name))))
(general-vmap :keymaps 'cider-mode-map "c" 'evil-change)

(provide 'cogent-clojure)
