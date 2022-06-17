;;; -*- lexical-binding: t -*-


;; most of the below config is from
;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org

;; in eshell, don't try to page things through less
(setenv "PAGER" "cat")

;; make `gst' open magit-status in the current directory
(defun eshell/gst (&rest args)
  (magit-status (pop args) nil)
  (eshell/echo))   ;; The echo command suppresses output

;; funky eshell prompt
(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (not (file-remote-p pwd))
             (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (require 'magit)
    (let* ((git-repo (if-let ((git-url (magit-get "remote.origin.url")))
                       (thread-first git-url string-trim file-name-base)
                       "???"))
           (git-branch (magit-get-current-branch))
           (git-icon  ""))
      (concat git-repo " " git-icon " " git-branch))))

;; The function takes the current directory passed in via pwd and
;; replaces the $HOME part with a tilde. I’m sure this function
;; already exists in the eshell source, but I didn’t find it…

(defun pwd-replace-home (pwd)
  "Replace home in PWD with tilde (~) character."
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

;; Make the directory name be shorter…by replacing all directory names
;; with just its first names. However, we leave the last two to be the
;; full names. Why yes, I did steal this.

(defun pwd-shorten-dirs (pwd)
  "Shorten all directory names in PWD except the last two."
  (let ((p-lst (split-string pwd "/")))
    (if (> (length p-lst) 2)
        (concat
         (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                               (substring elm 0 1)))
                    (butlast p-lst 2)
                    "/")
         "/"
         (mapconcat (lambda (elm) elm)
                    (last p-lst 2)
                    "/"))
      pwd)))  ;; Otherwise, we just return the PWD

;; Break up the directory into a “parent” and a “base”:

(defun split-directory-prompt (directory)
  (if (string-match-p ".*/.*" directory)
      (list (file-name-directory directory) (file-name-base directory))
    (list "" directory)))

;; Using virtual environments for certain languages is helpful to
;; know, especially since I change them based on the directory.

(defun ruby-prompt ()
  "Returns a string (may be empty) based on the current Ruby Virtual Environment."
  (let* ((executable "~/.rvm/bin/rvm-prompt")
         (command    (concat executable "v g")))
    (when (file-exists-p executable)
      (let* ((results (shell-command-to-string executable))
             (cleaned (string-trim results))
             (gem     (propertize "\xe92b" 'face `(:family "alltheicons"))))
        (when (and cleaned (not (equal cleaned "")))
          (replace-regexp-in-string
           (regexp-quote "ruby-") gem cleaned))))))

(defun python-prompt ()
  "Returns a string (may be empty) based on the current Python
   Virtual Environment. Assuming the M-x command: `pyenv-mode-set'
   has been called."
  (when (fboundp #'pyenv-mode-version)
    (let ((venv (pyenv-mode-version)))
      (when venv
        (concat
         (propertize "\xe928" 'face `(:family "alltheicons"))
         (pyenv-mode-version))))))

;; Now tie it all together with a prompt function can color each of the prompts components.

(defun eshell/eshell-local-prompt-function ()
  "A prompt for eshell that works locally (in that is assumes
that it could run certain commands) in order to make a prettier,
more-helpful local prompt."
  (interactive)
  (let* ((pwd        (eshell/pwd))
         (directory (split-directory-prompt
                     (pwd-shorten-dirs
                      (pwd-replace-home pwd))))
         (parent (car directory))
         (name   (cadr directory))
         (branch (curr-dir-git-branch-string pwd))
         (mac-p (string-equal system-type "darwin"))
         (ruby   (when (not (or mac-p (file-remote-p pwd))) (ruby-prompt)))
         (python (when (not (or mac-p (file-remote-p pwd))) (python-prompt)))

         (dark-env (eq 'dark (frame-parameter nil 'background-mode)))
         (for-bars                 `(:weight bold))
         (for-parent  (if dark-env `(:foreground "dark purple") `(:foreground "blue")))
         (for-dir     (if dark-env `(:foreground "purple" :weight bold)
                        `(:foreground "blue" :weight bold)))
         (for-git                  (if dark-env `(:foreground "pink")
                                     `(:foreground "dark magenta")))
         (for-ruby                 `(:foreground "red"))
         (for-python               `(:foreground "#5555FF")))

    (concat
     (propertize "⟣─ "    'face for-bars)
     (propertize parent   'face for-parent)
     (propertize name     'face for-dir)
     (when branch
       (concat (propertize " ── "    'face for-bars)
               (propertize branch   'face for-git)))
     (when ruby
       (concat (propertize " ── " 'face for-bars)
               (propertize ruby   'face for-ruby)))
     (when python
       (concat (propertize " ── " 'face for-bars)
               (propertize python 'face for-python)))
     (propertize "\n"     'face for-bars))))

(setq-default eshell-prompt-function #'eshell/eshell-local-prompt-function
              eshell-prompt-regexp "^⟣─ [^\n]*$")

;; Turn off the default prompt, otherwise, it won’t use ours:

(setq eshell-highlight-prompt nil)

(use-package vterm
  :commands (vterm vterm-other-window)
  :custom ((vterm-shell (executable-find "fish"))
           (vterm-buffer-name-string "vterm %s"))
  :config
  (custom-set-faces
   `(vterm-color-cyan ((t (:background "#2ea6ee" :foreground "#2ea6ee"))))
   `(vterm-color-white ((t (:background "#ffffff" :foreground "#ffffff"))))
   `(vterm-color-black ((t (:background "#000000" :foreground "#000000"))))
   `(vterm-color-green ((t (:background "#338e33" :foreground "#338e33")))))
  ;; make f3 pass through
  ;; start in emacs mode
  :hook (vterm-mode-hook . (lambda () (display-line-numbers-mode -1))))

;; (use-package helm-switch-shell
;;   :commands helm-switch-shell
;;   :straight (helm-switch-shell
;;              :type git
;;              :host github
;;              :repo "jamesnvc/helm-switch-shell")
;;   :custom ((helm-switch-shell-new-shell-type 'eshell)
;;            (helm-switch-shell-show-shell-indicator t)))

(use-package with-editor
  :hook ((eshell-mode-hook . with-editor-export-editor)
         (vterm-mode-hook . with-editor-export-editor)))

(require 'cogent-shell-switch)

(provide 'cogent-shell)
