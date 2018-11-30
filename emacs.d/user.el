;; -*- lexical-binding: t -*-

(set-frame-font "PragmataPro 7" nil t)
(require 'cogent-pragmata)

;; Quick way to jump here
(set-register ?e (cons 'file (concat dotfiles-dir "user.el")))

(general-define-key :keymaps 'global
                    "<f3>" #'eshell
                    "<f4>" #'calc
                    "<f5>" #'notmuch
                    "<f6>" #'elfeed
                    "<f9>" #'helm-bookmarks)

;; Leader key stuff
(defun cogent/quit-help-window ()
  (interactive)
  (when-let (help-win (get-buffer-window "*Help*"))
      (quit-window nil help-win)))

(cogent/leader-def
  :states '(normal visual)
  "q" #'cogent/quit-help-window
  "+" (lambda () (interactive) (cogent-fonts/update-font-size 1))
  "-" (lambda () (interactive) (cogent-fonts/update-font-size -1)))

(general-define-key
 :keymaps 'dired-mode-map
 ;; still like vim-vinegar
 "-" #'dired-jump
 ;; Make "jump backwards" act as I expect in dired
 "C-o" #'quit-window
 "C-c C-e" #'wdired-change-to-wdired-mode)

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
                    [escape] #'keyboard-quit)
(general-define-key :keymaps '(minibuffer-local-map
                               minibuffer-local-ns-map
                               minibuffer-local-completion-map
                               minibuffer-local-must-match-map
                               minibuffer-local-isearch-map)
                    :states nil
                    [escape] #'minibuffer-keyboard-quit)


;; Eshell
(add-hook 'eshell-first-time-mode-hook
          (lambda ()
            (evil-mc-mode -1)
            ;; Need to do this in the hook because eshell defines its keymap
            ;; in kind of a bizarre way
            (general-define-key :keymaps 'eshell-mode-map
                                [remap eshell-pcomplete] #'helm-esh-pcomplete
                                "M-r" #'helm-eshell-history)
            (display-line-numbers-mode -1)))

;; Fancy symbols
(push '("lambda" . 955) prettify-symbols-alist)
(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)

;; Arduino
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

;; Org

(setq cogent/org-capture-file (concat org-directory "/refile.org"))
(setq cogent/org-diary-file (concat org-directory "/diary.org"))
(set-register ?o (cons 'file org-default-notes-file))
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
(setq org-use-fast-todo-selection t)
(setq org-agenda-files (list (concat org-directory "/notes.org")
                             (concat org-directory "/bloom.org")
                             (concat org-directory "/notebook")))
(setq org-refile-targets '((nil . (:maxlevel . 9))))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Elfeed
(load (concat dotfiles-dir "feeds.el"))

;; Scheme indentition for my macros
(defun cogent/scheme-hook ()
  (put 'if-let 'scheme-indent-function 1))
(add-hook 'scheme-mode-hook #'cogent/scheme-hook)

;; Make Gnome unicode input method work for emacs as well
;; Doing this instead of C-x 8 RET so the Kaleidoscope unicode input
;; method works in Emacs too
(define-key global-map (kbd "C-S-u")
  (lambda ()
    (interactive)
    (let* ((input-chs (cl-loop for ch = (read-char)
                               until (= ch ?\s)
                               collect ch))
           (input-str (apply #'string input-chs))
           (input-num (string-to-number input-str 16)))
      (insert-char input-num))))

;; Make old Ubuntu shellcheck not complain with flycheck
(setq flycheck-shellcheck-follow-sources nil)

(add-hook 'objc-mode-hook (lambda () (setq c-basic-offset 4)))

(when (not (server-running-p))
  (server-start))
