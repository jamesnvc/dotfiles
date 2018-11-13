;;; -*- lexical-binding: t -*-

(require 'cogent-package)
(require 'cogent-keys)

(use-package mmm-mode
  :config
  (setq mmm-global-mode 'maybe)
  (mmm-add-classes
   '((prolog-javascript
      ;; would prefer js2-mode, but it explicitly doesn't do syntax
      ;; highlighting in mmm-mode
      :submode javascript-mode
      :front "{|javascript([^)]*)||[\n\r]+"
      :back "|}")))
  (mmm-add-mode-ext-class 'prolog-mode nil 'prolog-javascript))

(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))
(add-to-list 'auto-mode-alist '("\\.plt$" . prolog-mode))

(defun cogent/prolog-add-use-module (module predicates)
  "Add a use_module statement to the top of the current prolog file"
  (interactive "Mmodule name: \nMpredicates: ")

  (save-excursion
    (if-let (search-point
             (save-excursion
               (beginning-of-buffer)
               (re-search-forward
                (s-concat "^:- use_module(" (regexp-quote module))
                nil t)))
        (progn
          (goto-char search-point)
          (search-forward "[")
          (insert predicates ", "))
      (progn
        (unless (search-backward-regexp "^:- use_module" nil t)
          (search-backward-regexp "^:- module")
          (next-line))
        (search-forward "(")
        (backward-char)
        (forward-sexp)
        (next-line)
        (insert ":- use_module(" module ", [" predicates "]).\n")))))

(general-nvmap :keymaps '(prolog-mode-map)
  "\\ u" 'cogent/prolog-add-use-module)

(provide 'cogent-prolog)
