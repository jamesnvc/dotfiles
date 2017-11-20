;; -*- lexical-binding: t -*-

(set-frame-font "PragmataPro 14" nil t)
(require 'cogent-pragmata)

;; Fix path for mac
(add-to-list 'exec-path "/Users/james/bin")
(setenv "PATH" (concat "/Users/james/bin:" (getenv "PATH")))

;; Quick way to jump here
(set-register ?e (cons 'file (concat dotfiles-dir "user.el")))

(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

(global-set-key (kbd "<f4>") 'calc)
(global-set-key (kbd "<f5>") 'notmuch)
(global-set-key (kbd "<f6>") 'elfeed)

;; Make C-u inverse of C-d like vim & move universal-argument to M-u
;; (since that's upcase-word by default & we'll use vim bindings for
;; that anyway)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "M-u") 'universal-argument)

;; Fix Y behaviour in evil
(defun cogent/evil-yank-to-eol (&optional argument)
  (interactive "P")
  (let ((beg (point))
        (end (save-excursion
               (evil-end-of-line)
               (forward-char)
               (point))))
    (evil-yank beg end)))
(define-key evil-normal-state-map "Y" 'cogent/evil-yank-to-eol)

;; Like vim-vinegar
(define-key evil-normal-state-map "-" '(lambda ()
                                         (interactive)
                                         (dired (f-dirname (buffer-file-name)))))
(evil-define-key 'normal dired-mode-map "-" 'dired-up-directory)

(general-nmap :prefix "SPC"
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

;; Moving windows
(general-define-key :keymaps '(evil-normal-state-map
                               dired-mode-map)
                    :repeat t
                    "C-l" 'evil-window-right
                    "C-h" 'evil-window-left
                    "C-j" 'evil-window-down
                    "C-k" 'evil-window-up)

;; git bindings
(general-define-key
 :keymaps 'normal
 :jump t
 "]c" 'git-gutter+-next-hunk
 "[c" 'git-gutter+-previous-hunk)
(general-nmap :prefix "SPC"
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
(general-define-key :keymaps '(evil-normal-state-map evil-visual-state-map)
                    "<ESC>" 'keyboard-quit)
(general-define-key :keymaps '(minibuffer-local-map
                               minibuffer-local-map
                               minibuffer-local-ns-map
                               minibuffer-local-completion-map
                               minibuffer-local-must-match-map
                               minibuffer-local-isearch-map)
                    "<ESC>" 'minibuffer-keyboard-quit)

;; Like vim-fireplace
;; TODO: put this in cogent-clojure
(defun cogent/eval-last-sexp (&optional prefix)
  "Wrap `cider-eval-last-sexp' for evil-mode, by moving one character ahead"
  (interactive "P")
  (save-excursion
    (cogent/evil-forward-sexp)
    (forward-char)
    (cider-eval-last-sexp prefix)))

(defun cogent/eval-last-sexp-and-replace ()
  "Wrap `cider-eval-last-sexp-and-replace' for evil-mode, by moving one character ahead"
  (interactive)
  (save-excursion
    (cogent/evil-forward-sexp)
    (forward-char)
    (cider-eval-last-sexp-and-replace)))

(defun cogent/clojure-hook ()
  ;; TODO: would be nice to bind like in vim, but it seems bindings
  ;; like `cp' make `c-<operator>' not work
  (general-nmap :prefix "SPC"
                :keymaps 'clojure-mode-map
                "p" #'cogent/eval-last-sexp
                "!" #'cogent/eval-last-sexp-and-replace)
  (evil-define-key 'normal clojure-mode-map
    (kbd "] C-d") #'cider-find-var
    "K" #'cider-doc
    (kbd "M-r") #'(lambda () (interactive)
                    (cider-load-file (buffer-file-name)))))
(add-hook 'clojure-mode-hook #'cogent/clojure-hook)

;; Eshell
(global-set-key (kbd "<f3>") 'eshell)
(defun cogent/eshell-hook ()
  (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
  (define-key eshell-mode-map (kbd "M-r") #'helm-eshell-history)
  (evil-mc-mode -1))
(add-hook 'eshell-mode-hook #'cogent/eshell-hook)

;; Fancy symbols
(push '("lambda" . 955) prettify-symbols-alist)
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

(with-eval-after-load "company"
  (evil-define-key 'insert company-active-map (kbd "C-w") #'evil-delete-backward-word)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-w") #'evil-delete-backward-word))

;; Arduino
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

;; Dired
(with-eval-after-load "dired"
  ;; Make "jump backwards" act as I expect in dired
  (define-key dired-mode-map (kbd "C-o") 'quit-window))

;; Org
(general-nmap :prefix "SPC o"
  "a" 'org-agenda
  "c" 'org-capture)
(setq cogent/org-capture-file (concat org-directory "/refile.org"))
(setq cogent/org-diary-file (concat org-directory "/diary.org"))
(set-register ?o (cons 'file org-default-notes-file))
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
(setq org-use-fast-todo-selection t)
(setq org-agenda-files (list (concat org-directory "/notes.org")
                             (concat org-directory "/bloom.org")
                             "~/Dropbox/Apps/MobileOrg/mobileorg.org"))
(setq org-refile-targets '((nil . (:maxlevel . 9))))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Helm
(general-nmap :prefix "SPC"
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
                            (select-window (previous-window))
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

;; Mail

(with-eval-after-load "notmuch"
  (general-define-key :keymaps '(notmuch-search-mode-map)
                      "j" 'notmuch-search-next-thread
                      "k" 'notmuch-search-previous-thread
                      "g g" 'notmuch-search-first-thread
                      "G" 'notmuch-search-last-thread)

  (general-define-key :keymaps '(notmuch-search-mode-map
                                 notmuch-hello-mode-map)
                      "C-l" 'evil-window-right
                      "C-h" 'evil-window-left
                      "C-j" 'evil-window-down
                      "C-k" 'evil-window-up))

;; Elfeed
(load (concat dotfiles-dir "feeds.el"))

;; Scheme indentition for my macros
(defun cogent/scheme-hook ()
  (put 'if-let 'scheme-indent-function 1))
(add-hook 'scheme-mode-hook #'cogent/scheme-hook)

;; Make Gnome unicode input method work for emacs as well
;; Doing this instead of C-x 8 RET so the Kaleidoscope unicode input
;; method works in Emacs too
(require 's)
(define-key global-map (kbd "C-S-u")
  #'(lambda ()
      (interactive)
      (let* ((input-chs (cl-loop for ch = (read-char)
                                until (= ch ?\s)
                                collect ch ))
            (input-str (apply #'string input-chs))
            (input-num (string-to-number input-str 16)))
        (insert-char input-num))))
