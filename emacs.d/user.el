;; -*- lexical-binding: t -*-

(set-frame-font "PragmataPro 7" nil t)
(require 'cogent-pragmata)

;; Quick way to jump here
(set-register ?e (cons 'file (concat dotfiles-dir "user.el")))

(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
(general-define-key :keymaps 'normal
                    "j" 'evil-next-visual-line
                    "k" 'evil-previous-visual-line
                    "C-u" 'evil-scroll-up
                    "M-u" 'universal-argument)

(general-def "<menu>" 'helm-M-x)

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

(my-leader-def
  :states '(normal visual)
  "w" 'save-buffer
  "x" 'evil-delete-buffer
  "<SPC>" 'evil-ex
  "m" 'helm-M-x
  "T" 'helm-find-files
  "t" 'helm-projectile-find-file-dwim
  "s" 'helm-projectile-ag
  "b" 'helm-buffers-list
  "l" 'swiper-helm
  "q" 'cogent/quit-help-window
  "+" (lambda () (interactive) (cogent-fonts/update-font-size 1))
  "-" (lambda () (interactive) (cogent-fonts/update-font-size -1)))

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
 :states '(normal visual)
 ;; Fix Y behaviour in evil
 "Y" 'cogent/evil-yank-to-eol
 ;; Like vim-vinegar
 "-" #'(lambda ()
         (interactive)
         (dired (f-dirname (buffer-file-name))))
 ;; Like vim-unimpaired
 "[ <SPC>" 'cogent/line-above
 "] <SPC>" 'cogent/line-below)

(general-define-key
 :keymaps 'dired-mode-map
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
(general-nmap :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
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
    ((make-splitter-fn (name open-fn split-fn)
                         `(defun ,name (_candidate)
                            ;; Display buffers in new windows
                            (dolist (cand (helm-marked-candidates))
                              (select-window (,split-fn))
                              (,open-fn cand))
                            ;; Adjust size of windows
                            (balance-windows)))
     (generate-helm-splitter-funcs
      (op-type open-fn)
      (let* ((prefix (s-concat "helm-" op-type "-switch-to-new-"))
             (vert-split (intern (s-concat prefix "-vert-window")))
             (horiz-split (intern (s-concat prefix "-horiz-window"))))
        `(progn
           (make-splitter-fn ,vert-split ,open-fn split-window-right)

           (make-splitter-fn ,horiz-split ,open-fn split-window-below)

           (add-to-list
            (quote ,(intern (s-concat "helm-type-" op-type "-actions")))
            '(,(s-concat "Display " op-type "(s) in new vertical split(s) `C-v'" )
              . ,vert-split)
            'append)

           (add-to-list
            (quote ,(intern (s-concat "helm-type-" op-type "-actions")))
            '(,(s-concat "Display " op-type "(s) in new horizontal split(s) `C-s'" )
              . ,vert-split)
            'append)

           (defun ,(intern (s-concat "helm-" op-type "-switch-new-vert-window"))
               ()
             (interactive)
             (with-helm-alive-p
               (helm-exit-and-execute-action (quote ,vert-split))))

           (defun ,(intern (s-concat "helm-" op-type "-switch-new-horiz-window"))
               ()
             (interactive)
             (with-helm-alive-p
               (helm-exit-and-execute-action (quote ,horiz-split))))))))

  (generate-helm-splitter-funcs "buffer" switch-to-buffer)
  (generate-helm-splitter-funcs "file" find-file))

(general-def helm-buffer-map
  "C-v" #'helm-buffer-switch-new-vert-window
  "C-s" #'helm-buffer-switch-new-horiz-window)
(general-def helm-projectile-find-file-map
  "C-v" #'helm-file-switch-new-vert-window
  "C-s" #'helm-file-switch-new-horiz-window)
(general-def helm-find-files-map
  "C-v" #'helm-file-switch-new-vert-window
  "C-s" #'helm-file-switch-new-horiz-window)

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


;; Prolog
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

;; Make old Ubuntu shellcheck not complain with flycheck
(setq flycheck-shellcheck-follow-sources nil)
