;; -*- lexical-binding: t -*-

(set-frame-font "PragmataPro 16" nil t)
(add-to-list 'exec-path "/Users/james/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/Library/TeX/Distributions/TeXLive-2011.texdist/Contents/Programs/texbin")
(add-to-list 'exec-path "/Users/james/.node_modules_global/bin")
(add-to-list 'exec-path "/Users/james/.rvm/rubies/default/bin")
(setenv "PATH" (concat "/Users/james/.rvm/rubies/default/bin:/Users/james/.node_modules_global/bin:/usr/local/bin:/Users/james/bin:" (getenv "PATH")))
(require 'cogent-pragmata)
(global-prettify-symbols-mode -1)

;; Quick way to jump here
(set-register ?e (cons 'file (concat dotfiles-dir "user.el")))

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

(cogent/leader-def
  :states '(normal visual)
  "w" #'save-buffer
  "q" #'cogent/quit-help-window
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
 "-" (lambda ()
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

;; changing variable naming style
(require 's)

(defun cogent/change-word-at-point (f)
  (destructuring-bind (start . end) (bounds-of-thing-at-point 'symbol)
    (save-excursion
      (let ((text (buffer-substring-no-properties start end)))
        (while (s-matches? "^[^[:alpha:]]" text)
          (incf start)
          (setf text (buffer-substring-no-properties start end)))
        (delete-region start end)
        (insert (funcall f text))))))

(defun cogent/kebab-case ()
  (interactive)
  (cogent/change-word-at-point #'s-dashed-words))

(defun cogent/snake-case ()
  (interactive)
  (cogent/change-word-at-point #'s-snake-case))

(defun cogent/camel-case ()
  (interactive)
  (cogent/change-word-at-point #'s-lower-camel-case))

(defun cogent/camel-case-upper ()
  (interactive)
  (cogent/change-word-at-point #'s-upper-camel-case))


(general-nmap "c" (general-key-dispatch 'evil-change
                    :name cogent/change-word-case
                    "r-" #'cogent/kebab-case
                    "r_" #'cogent/snake-case
                    "rc" #'cogent/camel-case
                    "rC" #'cogent/camel-case-upper))
(general-vmap "c" 'evil-change)

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
(add-hook 'eshell-first-time-mode-hook
          (lambda ()
            (evil-mc-mode -1)
            ;; Need to do this in the hook because eshell defines its keymap
            ;; in kind of a bizarre way
            (general-define-key :keymaps 'eshell-mode-map
                                [remap eshell-pcomplete] 'helm-esh-pcomplete
                                "M-r" 'helm-eshell-history)))

;; Fancy symbols
(push '("lambda" . 955) prettify-symbols-alist)
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

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

(defun cogent/lectureify ()
  (interactive)
  (set-frame-font "PragmataPro 30" nil t)
  (use-package solarized-theme)
  (load-theme 'solarized-light t))
(general-define-key :keymaps 'global "<f10>" #'cogent/lectureify)

(server-start)
