;; -*- lexical-binding: t -*-

(set-frame-font "PragmataPro Liga 8" nil t)
(dolist (face '(default fixed-pitch))
  (set-face-attribute face nil :font "PragmataPro Liga-8"))
(set-face-attribute 'variable-pitch nil :font "Helvetica-8")
(require 'cogent-pragmata)

(defvar cogent/extra-path-dirs nil
  "Extra directories I want added to PATH")

(defun cogent/eshell-add-paths ()
  "Add extra dirs to eshell's path.
Shouldn't be necessary now, after using fish shell and exec-path-from-shell."
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

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Leader key stuff
(defun cogent/quit-help-window ()
  "Close any open help sort of window."
  (interactive)
  (when-let (help-win (or (get-buffer-window "*Help*")
                          (get-buffer-window "*lsp-help*")
                          (get-buffer-window "*Help Definition*")))
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
            (when (version< emacs-version "27.0")
              ;; Need to do this in the hook because eshell defines its keymap
              ;; in kind of a bizarre way
              (general-define-key :keymaps 'eshell-mode-map
                                  [remap eshell-pcomplete] #'helm-esh-pcomplete
                                  "M-r" #'helm-eshell-history))
            (display-line-numbers-mode -1)))

(when (version<= "27.0" emacs-version)
  (general-define-key :keymaps 'eshell-mode-map
                      "M-r" #'helm-eshell-history))
;; Fancy symbols
(push '("lambda" . 955) prettify-symbols-alist)
(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)

;; Arduino
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

;; Org

;; trying to figure out occasional agenda issue
;; it seems like when it fails it shows the wrong task in the agenda as TODO?
;; and when it's working, it shows the task with TODO changed to DONE with the highlight next to it & does the right thing on refresh
;; wrong says TODO Deadline Current
;; arg for org-agenda-todo is still nil
;; didn't see the "todo state was already todo though"
;; wrong seems to just do "TODO state changed to DONE"
;; or "TODO state changed to DONE; state was already TODO; Entry repeats: ..."
;; correct seems to do "TODO state changed to DONE; TODO state changed to TODO; Entry repeats: ..."

;; aah, a key clue! If I have the point in the todo.org buffer, on
;; another todo entry, when I hit =t d= in the agenda buffer, the
;; other entry that shows up is the one the point is on and then when
;; I hit =g=, it shows the entry I tried to complete as DONE
;; doesn't seem to happen with emacs -Q, so something in my setup must be causing this...

;; printing out org-state or next in org-todo makes the point move to
;; the original position in the buffer...but not when printing with
;; %S, only %s?!

;; commenting out (load-theme 'modus-operandi t) makes it work, but
;; starting with emacs -Q and loading just org & modus doesn't show
;; the behaviour...

;; TODO state changed to DONE
;; TODO state was already TODO
;; Entry repeats: DEADLINE: <2020-09-25 Fri +1d -0d>

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

(when (fboundp 'x-export-frames)
  (defun screenshot-svg ()
    "Save a screenshot of the current frame as an SVG."
    (interactive)
    (let* ((filename (make-temp-file "Emacs" nil ".svg"))
           (data (x-export-frames nil 'svg)))
      (with-temp-file filename
        (insert data))
      (kill-new filename)
      (message "Saved to '%s'" filename))))

(require 'server)
(when (not (server-running-p))
  (server-start))

(direnv-mode)
