;;; -*- lexical-binding: t -*-

(require 'cogent-package)
(require 'cogent-evil)

(use-package paredit
  :init
  ;; I'm going to manually set all the paredit keys
  ;; prevent paredit-define-keys from defining all its bindings
  (advice-add 'paredit-define-keys :override (lambda (&rest) nil)))

;; Copying vim-sexp bindings
(defun cogent/insert-at-end-of-sexp (&optional argument)
  "Go to the end of the current surrounding S-expression and enter insert mode"
  (interactive "P")
  (backward-up-list)
  (if (numberp argument)
      (forward-sexp argument)
    (forward-sexp))
  (backward-char)
  (evil-insert 0))

(defun cogent/insert-at-beginning-of-sexp (&optional argument)
  "Go to the beginning of the current containing sexp and enter insert mode"
  (interactive "P")
  (backward-up-list)
  (forward-char)
  (evil-insert 0))

(defun cogent/at-end-of-sexp-p ()
  (or (= ?\) (char-after (point)))
      (= ?  (char-after (1+ (point))))
      (= ?\n (char-after (1+ (point))))))

(defun cogent/evil-forward-sexp (&optional argument)
  "Wrapper around paredit-forward to take into account the fact that
we can't move past the last character in a line in normal mode"
  (interactive "P")
  (if (cogent/at-end-of-sexp-p) (forward-char))
  (paredit-forward argument)
  (backward-char))

(defun cogent/evil-forward-sexp-op (&optional argument)
  "Wrapper around paredit-forward to take into account the fact that
we can't move past the last character in a line in normal mode.
This version is for operator mode, where we want it to include the last paren"
  (interactive "P")
  (if (cogent/at-end-of-sexp-p) (forward-char))
  (paredit-forward argument))

(defun cogent/evil-forward-sexp-visual (&optional argument)
  "Wrapper around paredit-forward to take into account the fact that
we can't move past the last character in a line in normal mode.
This version is for visual mode"
  (interactive "P")
  (if (cogent/at-end-of-sexp-p) (forward-char))
  (backward-char)
  (paredit-forward argument))

(defun cogent/evil-backward-sexp (&optional argument)
  "Wrapper around paredit-backward to take into account the fact that
we can't move past the last character in a line in normal mode"
  (interactive "P")
  ;; XXX: need to move forward if at the end so we can go over multiple?
  (if (= ?\) (char-after (point)))
      (forward-char))
  (paredit-backward argument))

(defun cogent/evil-backward-sexp-op (&optional argument)
  "Wrapper around paredit-backward to take into account the fact that
we can't move past the last character in a line in normal mode.
This version is for operator mode, where we want it to include the paren."
  (interactive "P")
  (if (= ?\) (char-after (point)))
      (forward-char))
  (paredit-backward argument)
  (forward-char))

(defun cogent/evil-backward-sexp-visual (&optional argument)
  "Wrapper around paredit-backward to take into account the fact that
we can't move past the last character in a line in normal mode.
This version is for visual mode."
  (interactive "P")
  (if (= ?\) (char-after (point)))
      (forward-char))
  (paredit-backward argument))

(defun cogent/wrap-sexp-start (&optional argument)
  "Wrap the sexp under the cursor in parentheses and put the cursor in
insert mode at the beginning of the new sexp"
  (interactive "P")
  (save-excursion
    (insert (make-string (or argument 1) ?\())
    (paredit-forward)
    (insert (make-string (or argument 1) ?\))))
  (evil-append 0))

(defun cogent/wrap-sexp-end (&optional argument)
  "Wrap the sexp under the cursor in parentheses and put the cursor in
insert mode at the end of the new sexp"
  (interactive "P")
  (insert (make-string (or argument 1) ?\())
  (paredit-forward)
  (insert " " (make-string (or argument 1) ?\)))
  (backward-char)
  (evil-insert 0))

(defun cogent/wrap-braces-start (&optional argument)
  "Wrap the sexp under the cursor in braces ({}) and put the cursor in
insert mode at the beginning of the new sexp"
  (interactive "P")
  (save-excursion
    (insert (make-string (or argument 1) ?\{))
    (paredit-forward)
    (insert (make-string (or argument 1) ?\})))
  (evil-append 0))

(defun cogent/wrap-braces-end (&optional argument)
  "Wrap the sexp under the cursor in braces ({}) and put the cursor in
insert mode at the end of the new sexp"
  (interactive "P")
  (insert (make-string (or argument 1) ?\{))
  (paredit-forward)
  (insert " " (make-string (or argument 1) ?\}))
  (backward-char)
  (evil-insert 0))

(defun cogent/wrap-brackets-start (&optional argument)
  "Wrap the sexp under the cursor in brackets ([]) and put the cursor in
insert mode at the beginning of the new sexp"
  (interactive "P")
  (save-excursion
    (insert (make-string (or argument 1) ?\[))
    (paredit-forward)
    (insert (make-string (or argument 1) ?\])))
  (evil-append 0))

(defun cogent/wrap-brackets-end (&optional argument)
  "Wrap the sexp under the cursor in brackets ([]) and put the cursor in
insert mode at the end of the new sexp"
  (interactive "P")
  (insert (make-string (or argument 1) ?\[))
  (paredit-forward)
  (insert " " (make-string (or argument 1) ?\]))
  (backward-char)
  (evil-insert 0))

(defun cogent/paredit-indent (&optional argument)
  "Intelligently indent lisp-type code"
  (interactive "P")
  (let ((begin (point))
        end)
    (save-excursion
      (cogent/evil-forward-sexp)
      (setq end (point)))
    (evil-indent begin end)))

(defun cogent/paredit-vim-bindings ()
  "Evil bindings for paredit to imitate vim-sexp"
  (evil-define-key 'insert paredit-mode-map "(" #'paredit-open-round)
  (evil-define-key 'insert paredit-mode-map ")" #'paredit-close-round)
  (evil-define-key 'insert paredit-mode-map "[" #'paredit-open-square)
  (evil-define-key 'insert paredit-mode-map "]" #'paredit-close-square)
  (evil-define-key 'insert paredit-mode-map ";" #'paredit-semicolon)
  (evil-define-key 'visual paredit-mode-map "W" #'cogent/evil-forward-sexp-visual)
  (evil-define-key 'visual paredit-mode-map "B" #'cogent/evil-backward-sexp-visual)
  (evil-define-key 'operator paredit-mode-map "W" #'cogent/evil-forward-sexp-op)
  (evil-define-key 'operator paredit-mode-map "B" #'cogent/evil-backward-sexp-op)
  (evil-define-key 'normal paredit-mode-map "W" #'cogent/evil-forward-sexp)
  (evil-define-key 'normal paredit-mode-map "B" #'cogent/evil-backward-sexp)
  (evil-define-key 'normal paredit-mode-map "\\@" #'paredit-splice-sexp)
  (evil-define-key 'normal paredit-mode-map "\\o" #'paredit-raise-sexp)
  (evil-define-key 'normal paredit-mode-map ">)" #'paredit-forward-slurp-sexp)
  (evil-define-key 'normal paredit-mode-map "<)" #'paredit-forward-barf-sexp)
  (evil-define-key 'normal paredit-mode-map "<(" #'paredit-backward-slurp-sexp)
  (evil-define-key 'normal paredit-mode-map ">(" #'paredit-backward-barf-sexp)
  (evil-define-key 'normal paredit-mode-map ">i" #'cogent/insert-at-end-of-sexp)
  (evil-define-key 'normal paredit-mode-map "<i" #'cogent/insert-at-beginning-of-sexp)
  (evil-define-key 'normal paredit-mode-map "\\w" #'cogent/wrap-sexp-start)
  (evil-define-key 'normal paredit-mode-map "\\W" #'cogent/wrap-sexp-end)
  (evil-define-key 'normal paredit-mode-map "\\{" #'cogent/wrap-braces-start)
  (evil-define-key 'normal paredit-mode-map "\\}" #'cogent/wrap-braces-end)
  (evil-define-key 'normal paredit-mode-map "\\[" #'cogent/wrap-brackets-start)
  (evil-define-key 'normal paredit-mode-map "\\]" #'cogent/wrap-brackets-end)
  (evil-define-key 'normal paredit-mode-map "(" #'paredit-backward-up)
  (evil-define-key 'normal paredit-mode-map ")" #'paredit-forward-up)
  (evil-define-key 'normal paredit-mode-map "=" #'cogent/paredit-indent)

  (general-define-key :states 'insert
    ")" #'paredit-close-round
    "]" #'paredit-close-square
    "}" #'paredit-close-curly))

(add-hook 'paredit-mode-hook #'cogent/paredit-vim-bindings)

(provide 'cogent-sexp)
