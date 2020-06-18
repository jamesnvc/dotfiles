;; -*- lexical-binding: t -*-

(set-frame-font "PragmataPro 7" nil t)
(require 'cogent-pragmata)

(defvar cogent/extra-path-dirs nil
  "Extra directories I want added to PATH")

(defun cogent/eshell-add-paths ()
  (when cogent/extra-path-dirs
    (setq eshell-path-env (concat (s-join ":" cogent/extra-path-dirs)
                                  ":"
                                  eshell-path-env))))
(add-hook 'eshell-mode-hook #'cogent/eshell-add-paths)

(defun cogent/add-to-all-paths (dir)
  "Add directory to exec-path, $PATH and eshell-path-env"
  (cogent/after-path-init
   (add-to-list 'exec-path dir)
   (setenv "PATH" (concat dir ":" (getenv "PATH")))
   (add-to-list 'cogent/extra-path-dirs dir)))

(when (string-equal (system-name) "zhora.local")
  (set-frame-font "PragmataPro 14" nil t)
  (setq ns-antialias-text nil)
  (cogent/add-to-all-paths (expand-file-name "~/bin")))

(when (string-equal (system-name) "fuchikoma")
  (set-frame-font "PragmataPro 16" nil t)
  (global-prettify-symbols-mode -1)
  (mac-auto-operator-composition-mode 1)
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta))

(when (string-equal (system-name) "roy")
  (set-frame-font "PragmataPro 10" nil t)
  (cogent/add-to-all-paths (expand-file-name "~/.nix-profile/bin")))

(if (version< emacs-version "27.0")
    (general-define-key :keymaps 'global
                        "<f2>"   #'cogent/eyebrowse-helm)
  (general-define-key :keymaps 'global
                      "<f2>" #'tab-bar-select-tab-by-name))
(general-define-key :keymaps 'global
                    "<f3>"   #'helm-switch-shell
                    "<f4>"   #'calc
                    "<f5> 5" #'cogent/notmuch-inbox
                    "<f5> 4" #'helm-notmuch
                    "<f5> 3" #'notmuch
                    "<f6>"   #'elfeed
                    "<f7>"   #'cogent/lookup-word
                    "<f8>"   #'org-store-link
                    "<f9>"   #'helm-bookmarks)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

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

(with-eval-after-load "org"
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
  (setq org-use-fast-todo-selection t)
  (setq org-agenda-files (concat org-directory "/dir"))
  (setq org-agenda-use-tag-inheritance nil)
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-ignore-drawer-properties '(effort appt category))
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm))

;; Elfeed
(load (concat dotfiles-dir "feeds.el"))

;; Scheme indentation for my macros
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

;; ediff
(defun cogent/ediff-copy-both-to-C ()
  "Via
https://stackoverflow.com/questions/9656311/conflict-resolution-with-emacs-ediff-how-can-i-take-the-changes-of-both-version.
Take both changes in diff."
  (interactive)
  (ediff-copy-diff
   ediff-current-difference nil 'C nil
   (mapconcat
    (lambda (d) (ediff-get-region-contents ediff-current-difference d ediff-control-buffer))
    '(A B)
    "")))
(defun cogent/ediff-mode-hook ()
  (define-key ediff-mode-map "B" #'cogent/ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook #'cogent/ediff-mode-hook)

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(add-hook 'java-mode-hook (lambda () (setq c-basic-offset 4)))

(general-define-key :states '(normal)
                    ;; Evil overrides this to be something I never use
                    "M-." #'xref-find-definitions
                    ;; and paredit overrides this to be something I don't understand
                    "M-?" #'xref-find-references)

(require 'server)
(when (not (server-running-p))
  (server-start))

(direnv-mode)
