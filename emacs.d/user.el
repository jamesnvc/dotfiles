;; -*- lexical-binding: t -*-

(set-frame-font "PragmataPro 7" nil t)

(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

(global-set-key (kbd "<f4>") 'calc)

;; Make C-u inverse of C-d like vim & move universal-argument to M-u
;; (since that's upcase-word by default & we'll use vim bindings for
;; that anyway)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "M-u") 'universal-argument)

;; Fix Y behaviour in evil
(defun cogent/evil-yank-to-eol (&optional argument)
  (interactive "P")
  (let ((beg (point))
        end)
    (save-excursion
      (evil-end-of-line)
      (forward-char)
      (setq end (point)))
    (evil-yank beg end)))
(define-key evil-normal-state-map "Y" 'cogent/evil-yank-to-eol)

;; Like vim-vinegar
(define-key evil-normal-state-map "-" '(lambda ()
                                         (interactive)
                                         (dired (f-dirname (buffer-file-name)))))
(evil-define-key 'normal dired-mode-map "-" 'dired-up-directory)

(evil-leader/set-key
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
(define-key dired-mode-map (kbd "C-l") 'evil-window-right)
(define-key dired-mode-map (kbd "C-h") 'evil-window-left)
(define-key dired-mode-map (kbd "C-j") 'evil-window-down)
(define-key dired-mode-map (kbd "C-k") 'evil-window-up)

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
;; TODO: put this in cogent-clojure
(defun cogent/eval-last-sexp (&optional prefix)
  "Wrap `cider-eval-last-sexp' for evil-mode, by moving one character ahead"
  (interactive "P")
  (save-excursion
    (cogent/evil-forward-sexp)
    (cider-eval-last-sexp prefix)))

(defun cogent/eval-last-sexp-and-replace ()
  "Wrap `cider-eval-last-sexp-and-replace' for evil-mode, by moving one character ahead"
  (interactive)
  (save-excursion
    (cogent/evil-forward-sexp)
    (cider-eval-last-sexp-and-replace)))

(defun cogent/clojure-hook ()
  ;; TODO: would be nice to bind like in vim, but it seems bindings
  ;; like `cp' make `c-<operator>' not work
  (evil-leader/set-key-for-mode 'clojure-mode "p" #'cogent/eval-last-sexp)
  (evil-leader/set-key-for-mode 'clojure-mode "!" #'cogent/eval-last-sexp-and-replace)
  (evil-define-key 'normal clojure-mode-map (kbd "] C-d") #'cider-find-var)
  (evil-define-key 'normal clojure-mode-map "K" #'cider-doc)
  (evil-define-key 'normal clojure-mode-map (kbd "M-r") #'cider-refresh))
(add-hook 'clojure-mode-hook #'cogent/clojure-hook)

;; Eshell
(global-set-key (kbd "<f3>") 'eshell)

;; Fancy symbols
(setq prettify-symbols-alist
      '(("lambda" . 955)))
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

(with-eval-after-load "company"
  (evil-define-key 'insert company-active-map (kbd "C-w") #'evil-delete-backward-word)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-w") #'evil-delete-backward-word))

;; Arduino
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

;; Org
(evil-leader/set-key
  "o a" 'org-agenda
  "o c" 'org-capture)
(setq cogent/org-capture-file (concat org-directory "/refile.org"))
(setq cogent/org-diary-file (concat org-directory "/diary.org"))
(set-register ?o (cons 'file org-default-notes-file))
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
(setq org-use-fast-todo-selection t)
(setq org-agenda-files (list (concat org-directory "/notes.org")))
(setq org-refile-targets '((nil . (:maxlevel . 3))))

;; Helm
(evil-leader/set-key
  ;; like Denite
  "T" 'helm-find-files
  "t" 'helm-projectile-find-file-dwim
  "s" 'helm-projectile-ag
  "b" 'helm-buffers-list
  "l" 'swiper-helm)

;; Enable opening helm results in splits
(cl-macrolet
    ((make-helm-splitter (name open-fn split-fn)
                         `(defun ,name (_candidate)
                            (require 'winner)
                            (select-window (car (last (winner-sorted-window-list))))
                            ;; Display buffers in new windows
                            (dolist (cand (helm-marked-candidates))
                              (select-window (,split-fn))
                              (,open-fn cand))
                            ;; Adjust size of windows
                            (balance-windows))))

  (make-helm-splitter helm-buffer-switch-to-new-vert-window
                      switch-to-buffer split-window-right)

  (make-helm-splitter helm-buffer-switch-to-new-horiz-window
                      switch-to-buffer split-window-below)

  (make-helm-splitter helm-file-switch-to-new-vert-window
                      find-file split-window-right)

  (make-helm-splitter helm-file-switch-to-new-horiz-window
                      find-file split-window-below))

(add-to-list 'helm-type-buffer-actions
             '("Display buffer(s) in new vertical split(s) `C-v'" .
               helm-buffer-switch-to-new-vert-window) 'append)

(add-to-list 'helm-type-buffer-actions
             '("Display buffer(s) in new horizontal split(s) `C-s'" .
               helm-buffer-switch-to-new-horiz-window) 'append)

(add-to-list 'helm-type-file-actions
             '("Display buffer(s) in new vertical split(s) `C-v'" .
               helm-file-switch-to-new-vert-window) 'append)

(add-to-list 'helm-type-file-actions
             '("Display buffer(s) in new horizontal split(s) `C-s'" .
               helm-file-switch-to-new-horiz-window) 'append)

(defun helm-buffer-switch-new-vert-window ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-buffer-switch-to-new-vert-window)))

(defun helm-buffer-switch-new-horiz-window ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-buffer-switch-to-new-horiz-window)))

(defun helm-file-switch-new-vert-window ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-file-switch-to-new-vert-window)))

(defun helm-file-switch-new-horiz-window ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-file-switch-to-new-horiz-window)))

(define-key helm-buffer-map (kbd "C-v") #'helm-buffer-switch-new-vert-window)
(define-key helm-buffer-map (kbd "C-s") #'helm-buffer-switch-new-horiz-window)
(define-key helm-projectile-find-file-map (kbd "C-v") #'helm-file-switch-new-vert-window)
(define-key helm-find-files-map (kbd "C-v") #'helm-file-switch-new-vert-window)
(define-key helm-projectile-find-file-map (kbd "C-s") #'helm-file-switch-new-horiz-window)
(define-key helm-find-files-map (kbd "C-s") #'helm-file-switch-new-horiz-window)
