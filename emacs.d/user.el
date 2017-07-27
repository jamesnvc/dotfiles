;; -*- lexical-binding: t -*-

(set-frame-font "PragmataPro Mono 7" nil t)

(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

(define-key evil-normal-state-map "-" 'dired)

(evil-leader/set-key
      "t" 'helm-find-files
      "o" 'helm-buffers-list
      "w" 'save-buffer
      "<SPC>" 'evil-ex
      "g s" 'magit-status
      "m" 'helm-M-x)

(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)

(evil-define-key 'normal emacs-lisp-mode-map (kbd "] C-d") 'find-function-at-point)

;; TODO: indent >> << bindings

;; git bindings
(define-key evil-normal-state-map (kbd "]c") 'git-gutter+-next-hunk)
(define-key evil-normal-state-map (kbd "[c") 'git-gutter+-previous-hunk)
(evil-leader/set-key "h s" 'git-gutter+-stage-hunks)
(evil-leader/set-key "g w" 'magit-stage-file)
(evil-leader/set-key "g c" 'magit-commit)

;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

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

(defun cogent/insert-at-begining-of-sexp (&optional argument)
  "Go to the begining of the current containing sexp and enter insert mode"
  (interactive "P")
  (backward-up-list)
  (forward-char)
  (evil-insert 0))

(defun cogent/evil-forward-sexp (&optional argument)
  "Wrapper around paredit-forward to take into account the fact that
we can't move past the last character in a line in normal mode"
  (interactive "P")
  (if (= ?\) (char-after (point)))
      (forward-char))
  (paredit-forward argument))

(defun cogent/evil-backward-sexp (&optional argument)
  "Wrapper around paredit-backward to take into account the fact that
we can't move past the last character in a line in normal mode"
  (interactive "P")
  ;; XXX: need to move forward if at the end so we can go over multiple?
  (if (= ?\) (char-after (point)))
      (forward-char))
  (paredit-backward argument))

(defun cogent/wrap-sexp-start (&optional argument)
  "Wrap the sexp under the cursor in paretheses and put the cursor in
insert mode at the begining of the new sexp"
  (interactive "P")
  ;; TODO: use argument?
  (save-excursion
    (insert "(")
    (paredit-forward)
    (insert ")"))
  (evil-append 0))

(defun cogent/wrap-sexp-end (&optional argument)
  "Wrap the sexp under the cursor in paretheses and put the cursor in
insert mode at the end of the new sexp"
  (interactive "P")
  ;; TODO: use argument?
  (insert "(")
  (paredit-forward)
  (insert " )")
  (backward-char)
  (evil-insert 0))

(defun cogent/paredit-vim-bindings ()
  "Evil bindings for paredit to imitate vim-sexp"
  (evil-define-key 'normal paredit-mode-map "W" 'cogent/evil-forward-sexp)
  (evil-define-key 'normal paredit-mode-map "B" 'cogent/evil-backward-sexp)
  (evil-define-key 'normal paredit-mode-map "\\@" 'paredit-splice-sexp)
  (evil-define-key 'normal paredit-mode-map "\\o" 'paredit-raise-sexp)
  (evil-define-key 'normal paredit-mode-map ">)" 'paredit-forward-slurp-sexp)
  (evil-define-key 'normal paredit-mode-map "<)" 'paredit-forward-barf-sexp)
  (evil-define-key 'normal paredit-mode-map "<(" 'paredit-backward-slurp-sexp)
  (evil-define-key 'normal paredit-mode-map ">(" 'paredit-backward-barf-sexp)
  (evil-define-key 'normal paredit-mode-map ">i" 'cogent/insert-at-end-of-sexp)
  (evil-define-key 'normal paredit-mode-map "<i" 'cogent/insert-at-begining-of-sexp)
  (evil-define-key 'normal paredit-mode-map "\\w" 'cogent/wrap-sexp-start)
  (evil-define-key 'normal paredit-mode-map "\\W" 'cogent/wrap-sexp-end)
  (evil-define-key 'normal paredit-mode-map "(" 'paredit-backward-up)
  (evil-define-key 'normal paredit-mode-map ")" 'paredit-forward-up)
  ;; TODO: operator-pending bindings for forward/backward sexp W/B
  ))

(add-hook 'paredit-mode-hook 'cogent/paredit-vim-bindings)

(defun cogent/clojure-hook ()
  (evil-define-key 'normal clojure-mode-map "cpp" 'monroe-eval-expression-at-point))

(add-hook 'clojure-mode-hook 'cogent-clojure-hook)
