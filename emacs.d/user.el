;; -*- lexical-binding: t -*-

(set-frame-font "PragmataPro Liga 8" nil t)
(dolist (face '(default fixed-pitch))
  (set-face-attribute face nil :font "PragmataPro Liga-8"))
(set-face-attribute 'variable-pitch nil :font "Helvetica-8")
(require 'cogent-pragmata)

(setq redisplay-dont-pause t)

(defvar cogent/extra-path-dirs nil
  "Extra directories I want added to PATH")

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

(general-define-key :keymaps 'global
                    "<f2>" (if (version< emacs-version "27.0")
                               #'cogent/eyebrowse-helm
                             #'tab-bar-select-tab-by-name)
                    "<f3>"   #'helm-switch-shell
                    "<f4>"   #'calc
                    "<f5> 5" #'cogent/notmuch-inbox
                    "<f5> 4" #'helm-notmuch
                    "<f5> 3" #'notmuch
                    "<f6>"   #'elfeed
                    "<f7>"   #'cogent/lookup-word
                    "<f8>"   #'org-store-link
                    "<f9>"   #'helm-bookmarks)

;; Leader key stuff
(require 'rx)
(defun cogent/quit-special-window ()
  "Close any open *special* window."
  (interactive)
  (loop for win being the windows of (selected-frame)
        for buff-name = (buffer-name (window-buffer win))
        when (and
              (not (string= buff-name "*scratch*"))
              (string-match-p
               (rx bos "*" (+ anychar) "*" (? "<" (+ digit) ">") eos)
               buff-name))
        collect win into special-wins
        finally do (mapcar (lambda (w) (quit-window nil w)) special-wins)))

(cogent/leader-def
  :states '(normal visual)
  "q" #'cogent/quit-special-window
  "+" (lambda () (interactive) (cogent-fonts/update-font-size 1))
  "-" (lambda () (interactive) (cogent-fonts/update-font-size -1)))

(use-package dired
  :straight (:type built-in)
  :bind
  (:map dired-mode-map
        ("-" . dired-jump)
        ("C-o" . quit-window)
        ("C-c C-e" . wdired-change-to-wdired-mode)))


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

(use-package eshell
  :config
  (defun cogent/eshell-first-time-setup ()
    (evil-mc-mode -1)
    (display-line-numbers-mode -1))

  (defun cogent/eshell-add-paths ()
    "Add extra dirs to eshell's path.
Shouldn't be necessary now, after using fish shell and exec-path-from-shell."
    (when cogent/extra-path-dirs
      (setq eshell-path-env (concat (string-join cogent/extra-path-dirs ":")
                                    ":"
                                    eshell-path-env))))
  :hook
  ((eshell-first-time-mode-hook . cogent/eshell-first-time-setup)
   (eshell-mode-hook . cogent/eshell-add-paths))
  :bind
  (:map eshell-mode-map
        ("M-r" . helm-eshell-history))
  (:map eshell-hist-mode-map
        ("M-r" . helm-eshell-history)))

;; Fancy symbols
(cl-pushnew '("lambda" . 955) prettify-symbols-alist)

;; Arduino
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

;; Org

(with-eval-after-load "org"
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
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (add-hook 'org-agenda-mode-hook (lambda () (display-line-numbers-mode 0))))

;; Elfeed
(load (concat dotfiles-dir "feeds.el"))

;; Scheme indentation for my macros
(defun cogent/scheme-hook ()
  (put 'if-let 'scheme-indent-function 1))

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


;; ediff
(use-package ediff
  :straight (:type built-in)
  :config
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
  :hook
  (ediff-keymap-setup-hook . cogent/ediff-mode-hook))

(general-define-key :states '(normal)
                    ;; Evil overrides this to be something I never use
                    "M-." #'xref-find-definitions
                    ;; and paredit overrides this to be something I don't understand
                    "M-?" #'xref-find-references)

(when (fboundp 'x-export-frames)
  (defun cogent/screenshot (type)
    "Save a screenshot of the current frame."
    (interactive (list (completing-read "Type: "
                                        '("svg" "pdf" "postscript" "png"))))
    (let* ((filename (make-temp-file "Emacs" nil (concat "." type)))
           (data (x-export-frames nil (intern type))))
      (with-temp-file filename
        (insert data))
      (kill-new filename)
      (message "Saved to '%s'" filename))))

(use-package emacs
  :config
  (minibuffer-depth-indicate-mode 1)
  :custom
  ((enable-recursive-minibuffers t))
  :hook
  ((emacs-lisp-mode-hook . prettify-symbols-mode)
   (objc-mode-hook . (lambda () (setq c-basic-offset 4)))
   (java-mode-hook . (lambda () (setq c-basic-offset 4)))
   (scheme-mode-hook . cogent/scheme-hook)
   (after-save-hook . executable-make-buffer-file-executable-if-script-p)
   (before-save-hook . time-stamp)))

(global-set-key (kbd "M-SPC") #'cycle-spacing)

(require 'server)
(when (not (server-running-p))
  (server-start))

(direnv-mode)
