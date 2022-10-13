;;; -*- lexical-binding: t -*-

(require 'cogent-package)
(require 'cogent-keys)

(use-package mmm-mode
  :straight (mmm-mode
             :type git
             :host github
             :repo "emacs-straight/mmm-mode")
  :config
  ;; mmm mode in Prolog breaks semantic highlighting, it seems?
  (setq mmm-global-mode nil) ;'maybe
  (mmm-add-classes
   '((prolog-javascript
      ;; would prefer js2-mode, but it explicitly doesn't do syntax
      ;; highlighting in mmm-mode
      :submode javascript-mode
      :front "{|javascript([^)]*)||[\n\r]+"
      :back "|}")))
  (mmm-add-mode-ext-class 'prolog-mode nil 'prolog-javascript))

;; Using sweep instead
;; (add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
;; (add-to-list 'auto-mode-alist '("\\.plt\\'" . prolog-mode))
;; Logtalk
(add-to-list 'auto-mode-alist '("\\.lgt\\'" . prolog-mode))

(defun cogent/prolog-add-use-module (module predicates)
  "Add a use_module statement to the top of the current prolog file"
  (interactive "Mmodule name: \nMpredicates: ")

  (save-excursion
    (if-let (search-point
             (save-excursion
               (beginning-of-buffer)
               (re-search-forward
                (concat "^:- use_module(" (regexp-quote module))
                nil t)))
        (progn
          (goto-char search-point)
          (search-forward "[")
          (insert predicates ", "))
      (progn
        (unless (search-backward-regexp "^:- use_module" nil t)
          (search-backward-regexp "^:- module")
          (forward-line))
        (search-forward "(")
        (backward-char)
        (forward-sexp)
        (forward-line)
        (insert ":- use_module(" module ", [" predicates "]).\n")))))

(defun cogent/prolog-add-docstring ()
  (interactive)
  (let (name args end-pos)
    (goto-char (prolog-pred-start))
    (save-mark-and-excursion
      (let ((name-start (point))
            (name-end (progn (search-forward "(") (backward-char) (point)))
            ;; [TODO] use tree-sitter to count args?
            (nargs (1+ (count-matches "," (point) (progn (forward-sexp)
                                                         (point))))))
        (setq name (buffer-substring-no-properties name-start name-end))
        (setq args nargs)))
    (forward-line -1) (end-of-line nil)
    (insert "\n%! " name "(")
    (setq end-pos (point))
    (insert (string-join (make-list args "_") ", "))
    (insert ") is det.\n%\n%  TODO")
    (goto-char end-pos)))

(general-nvmap :keymaps '(prolog-mode-map)
  "\\ u" #'cogent/prolog-add-use-module
  "\\ d" #'cogent/prolog-add-docstring)

(use-package ob-prolog
  :after org)

(use-package sweeprolog
  :defer t
  :commands (sweeprolog-mode)
  :straight (sweeprolog
             :type git
             :host nil
             :repo "https://git.sr.ht/~eshel/sweep"
             :branch "main"
             :files ("*.el" "sweep.pl"))
  :mode (("\\.pl\\'" . sweeprolog-mode)
         ("\\.plt\\'" . sweeprolog-mode))
  :config
  (setopt sweeprolog-swipl-path (expand-file-name "~/.local/bin/swipl")))

(provide 'cogent-prolog)
