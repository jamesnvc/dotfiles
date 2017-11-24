;; -*- lexical-binding: t -*-

(set-frame-font "PragmataPro 8" nil t)
(require 'cogent-pragmata)

;; Quick way to jump here
(set-register ?e (cons 'file (concat dotfiles-dir "user.el")))

(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
(general-define-key :keymaps 'normal
                    "j" 'evil-next-visual-line
                    "k" 'evil-previous-visual-line
                    "C-u" 'evil-scroll-up
                    "M-u" 'universal-argument)

(general-define-key :keymaps 'global
                    "<f3>" 'eshell
                    "<f4>" 'calc
                    "<f5>" 'notmuch
                    "<f6>" 'elfeed)

;; Leader key stuff
(defun cogent/quit-help-window ()
  (interactive)
  (when-let (help-win (get-buffer-window "*Help*"))
      (quit-window nil help-win)))

(general-nvmap :prefix "SPC"
               "w" 'save-buffer
               "x" 'evil-delete-buffer
               "<SPC>" 'evil-ex
               "m" 'helm-M-x
               "T" 'helm-find-files
               "t" 'helm-projectile-find-file-dwim
               "s" 'helm-projectile-ag
               "b" 'helm-buffers-list
               "l" 'swiper-helm
               "q" 'cogent/quit-help-window)

(defun cogent/evil-yank-to-eol (&optional argument)
  "Yank from point to end of line; like the behaviour I prefer `Y' in
evil to have."
  (interactive "P")
  (let ((beg (point))
        (end (save-excursion
               (evil-end-of-line)
               (forward-char)
               (point))))
    (evil-yank beg end)))

(defun cogent/line-below (&optional argument)
  "New blank line below the current line; like vim-unimpaired."
  (interactive "P")
  (save-excursion
    (dotimes (_ (or argument 1))
      (evil-insert-newline-below))))

(defun cogent/line-above (&optional argument)
  "New blank line above the current line; like vim-unimpaired."
  (interactive "P")
  (save-excursion
    (dotimes (_ (or argument 1))
      (evil-insert-newline-above))))

(general-define-key
 ;; Fix Y behaviour in evil
 "Y" 'cogent/evil-yank-to-eol
 ;; Like vim-vinegar
 "-" #'(lambda ()
         (interactive)
         (dired (f-dirname (buffer-file-name))))
 ;; Like vim-unimpaired
 "[ <SPC>" 'cogent/line-above
 "] <SPC>" 'cogent/line-below)

(general-define-key :keymaps 'dired-mode-map
                    ;; still like vim-vinegar
                    "-" 'dired-up-directory
                    ;; Make "jump backwards" act as I expect in dired
                    "C-o" 'quit-window)


;; Emacs-lisp
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
(general-nmap :keymaps 'emacs-lisp-mode-map
              ;; Like vim-unimpaired
              "] C-d" 'find-function-at-point
              "c" (general-key-dispatch 'evil-change
                    :name cogent/elisp-change-dispatch
                    "pp" 'cogent/elisp-eval-next-sexp
                    "p!" 'cogent/elisp-eval-and-replace-next-sexp
                    "c" 'evil-change-whole-line))
(general-vmap :keymaps 'emacs-lisp-mode-map "c" 'evil-change)

;; Moving windows
(general-define-key :keymaps '(normal
                               dired-mode-map
                               notmuch-search-mode-map
                               notmuch-hello-mode-map)
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
(general-nvmap :prefix "SPC h"
              "s" 'git-gutter+-stage-hunks)
(general-nvmap :prefix "SPC g"
              "s" 'magit-status
              "w" 'magit-stage-file
              "c" 'magit-commit
              "H" 'magit-log-buffer-file)

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
                    [escape] 'keyboard-quit)
(general-define-key :keymaps '(minibuffer-local-map
                               minibuffer-local-ns-map
                               minibuffer-local-completion-map
                               minibuffer-local-must-match-map
                               minibuffer-local-isearch-map)
                    :states nil
                    [escape] 'minibuffer-keyboard-quit)


;; Eshell
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (evil-mc-mode -1)
              ;; Need to do this in the hook because eshell defines its keymap
              ;; in kind of a bizarre way
              (general-define-key :keymaps 'eshell-mode-map
                                  [remap eshell-pcomplete] 'helm-esh-pcomplete
                                  "M-r" 'helm-eshell-history)))

;; Fancy symbols
(push '("lambda" . 955) prettify-symbols-alist)
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

(general-define-key :keymaps 'company-active-map
                    "C-n" 'company-select-next
                    "C-p" 'company-select-previous
                    "C-w" 'evil-delete-backward-word)
(general-define-key :keymaps 'company-active-map
                    :states 'insert
                    "C-w" 'evil-delete-backward-word)

;; Arduino
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

;; Org
(general-nvmap :prefix "SPC o"
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



;; Enable opening helm results in splits
(cl-macrolet
    ((make-helm-splitter (name open-fn split-fn)
                         `(defun ,name (_candidate)
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
(general-define-key :keymaps '(notmuch-search-mode-map)
                    "j" 'notmuch-search-next-thread
                    "k" 'notmuch-search-previous-thread
                    "g g" 'notmuch-search-first-thread
                    "G" 'notmuch-search-last-thread)

;; Elfeed
(load (concat dotfiles-dir "feeds.el"))

;; Scheme indentition for my macros
(defun cogent/scheme-hook ()
  (put 'if-let 'scheme-indent-function 1))
(add-hook 'scheme-mode-hook #'cogent/scheme-hook)

;; markdown
(add-hook 'markdown-mode-hook #'(lambda () (flyspell-mode 1)))

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
