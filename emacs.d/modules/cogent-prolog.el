;;; -*- lexical-binding: t -*-

(require 'cogent-package)
(require 'cogent-keys)

(use-package mmm-mode
  :straight (mmm-mode
             :type git
             :host github
             :repo "emacs-straight/mmm-mode")
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

(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
(add-to-list 'auto-mode-alist '("\\.plt\\'" . prolog-mode))

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

(defun cogent/prolog-add-docstring ()
  (interactive)
  (let (name args end-pos)
    (prolog-beginning-of-predicate)
    (save-mark-and-excursion
      (let ((name-start (point))
            (name-end (progn (search-forward "(") (backward-char) (point)))
            ;; [TODO] use tree-sitter to count args?
            (nargs (1+ (count-matches "," (point) (progn (forward-sexp)
                                                         (point))))))
        (setq name (buffer-substring-no-properties name-start name-end))
        (setq args nargs)))
    (forward-line -1)
    (insert "\n%! " name "(")
    (setq end-pos (point))
    (insert (->> (-repeat args "_") (s-join ", ")))
    (insert ") is det.\n%\n%  TODO")
    (goto-char end-pos)))

(general-nvmap :keymaps '(prolog-mode-map)
  "\\ u" #'cogent/prolog-add-use-module
  "\\ d" #'cogent/prolog-add-docstring)

(use-package ob-prolog
  :after org)

(provide 'cogent-prolog)
