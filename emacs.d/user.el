;; -*- lexical-binding: t -*-

(set-frame-font "PragmataPro 7" nil t)

(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

;; Make C-u inverse of C-d like vim & move universal-argument to M-u
;; (since that's upcase-word by default & we'll use vim bindings for
;; that anyway)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "M-u") 'universal-argument)

;; Like vim-vinegar
(define-key evil-normal-state-map "-" 'dired)

(evil-leader/set-key
  ;; like Denite
  "T" 'helm-find-files
  "t" 'helm-projectile-find-file-dwim
  "s" 'helm-projectile-ag
  "o" 'helm-buffers-list
  "w" 'save-buffer
  ;; misc to make command mode easier
  "<SPC>" 'evil-ex
  "m" 'helm-M-x
  "x" 'evil-delete-buffer)

;; Like vim-unimpaired
(evil-define-key 'normal emacs-lisp-mode-map (kbd "] C-d") 'find-function-at-point)
(defun cogent/line-below (&optional argument)
  (interactive "P")
  (save-excursion
    (dotimes (_ (or argument 1))
      (evil-insert-newline-below))))
(define-key evil-normal-state-map (kbd "] <SPC>") 'cogent/line-below)
(defun cogent/line-above (&optional argument)
  (interactive "P")
  (save-excursion
    (dotimes (_ (or argument 1))
      (evil-insert-newline-above))))
(define-key evil-normal-state-map (kbd "[ <SPC>") 'cogent/line-above)

;; TODO: indent >> << bindings

;; Moving windows
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)

;; git bindings
(define-key evil-normal-state-map (kbd "]c") 'git-gutter+-next-hunk)
(define-key evil-normal-state-map (kbd "[c") 'git-gutter+-previous-hunk)
(evil-leader/set-key
  "h s" 'git-gutter+-stage-hunks
  "g s" 'magit-status
  "g w" 'magit-stage-file
  "g c" 'magit-commit)

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

;; Like vim-fireplace
(defun cogent/clojure-hook ()
  (evil-define-key 'normal clojure-mode-map "cpp" 'monroe-eval-expression-at-point))
(add-hook 'clojure-mode-hook 'cogent-clojure-hook)

;; Eshell
(global-set-key (kbd "<f3>") 'eshell)
