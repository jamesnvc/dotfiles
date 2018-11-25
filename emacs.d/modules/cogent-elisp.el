;;; -*- lexical-binding: t -*-

(require 'cogent-package)
(require 'cogent-keys)

(use-package paredit
  :commands paredit-mode enable-paredit-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  :diminish paredit-mode)

(use-package highlight-parentheses
  :commands highlight-parentheses-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
  :diminish highlight-parentheses-mode)

(defun cogent-elisp/remove-elc-on-save ()
  "If you're saving an elisp file, you probably want to remove the
now-invalid elc file"
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (when (file-exists-p (concat buffer-file-name "c"))
                (delete-file (concat buffer-file-name "c"))))))
(add-hook 'emacs-lisp-mode-hook #'cogent-elisp/remove-elc-on-save)

;; Show context-based docs in minibuffer
(add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)

;; Show the result of evaluating as an overlay in the elisp buffer
(use-package eros
  :commands eros-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'eros-mode))

(defun cogent/elisp-eval-next-sexp ()
  (interactive)
  (save-excursion
    (cogent/evil-forward-sexp)
    (forward-char)
    (eros-eval-last-sexp nil)))

(defun cogent/elisp-eval-and-replace-next-sexp ()
  (interactive)
  (let ((start (point))
        end)
    (save-excursion
      (cogent/evil-forward-sexp)
      (forward-char)
      (setq end (point))
      (eros-eval-last-sexp t)
      (delete-region start end))))

(general-nmap :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
  ;; Like vim-unimpaired
  "] C-d" 'find-function-at-point
  "c" (general-key-dispatch 'evil-change
        :name cogent/elisp-change-dispatch
        "pp" #'cogent/elisp-eval-next-sexp
        "p!" #'cogent/elisp-eval-and-replace-next-sexp
        "c" #'evil-change-whole-line
        "r-" #'cogent/kebab-case
        "r_" #'cogent/snake-case
        "rc" #'cogent/camel-case
        "rC" #'cogent/camel-case-upper))
(general-vmap :keymaps 'emacs-lisp-mode-map "c" 'evil-change)


(provide 'cogent-elisp)
