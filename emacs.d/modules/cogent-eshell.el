;;; -*- lexical-binding: t -*-


;; most of the below config is from
;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org

;; in eshell, don't try to page things through less
(setenv "PAGER" "cat")

;; Make T function as an eshell glob to find org files with a matching tag
;; e.g. `ls *.org(T'git')`'
(defun eshell-org-file-tags ()
  "Helps the eshell parse the text the point is currently on,
looking for parameters surrounded in single quotes. Returns a
function that takes a FILE and returns nil if the file given to
it doesn't contain the org-mode #+TAGS: entry specified."
  (if (looking-at "'\\([^)']+\\)'")
      (let* ((tag (match-string 1))
             (reg (concat "^#\\+TAGS: .*" tag "\\b")))
        (goto-char (match-end 0))
        `(lambda (file)
           (with-temp-buffer
             (insert-file-contents file)
             (re-search-forward ,reg nil t 1))))
    (error "The `T' predicate takes an org-mode tag value in single quotes.")))
(add-hook 'eshell-pred-load-hook (lambda ()
  (add-to-list 'eshell-predicate-alist '(?T . (eshell-org-file-tags)))))

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
    (let* ((git-url (shell-command-to-string "git config --get remote.origin.url"))
           (git-repo (file-name-base (s-trim git-url)))
           (git-output (shell-command-to-string (concat "git rev-parse --abbrev-ref HEAD")))
           (git-branch (s-trim git-output))
           (git-icon  "\xe0a0"))
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

         (dark-env (eq 'dark (frame-parameter nil 'background-mode)))
         (for-bars                 `(:weight bold))
         (for-parent  (if dark-env `(:foreground "dark purple") `(:foreground "blue")))
         (for-dir     (if dark-env `(:foreground "purple" :weight bold)
                        `(:foreground "blue" :weight bold)))
         (for-git                  `(:foreground "pink")))

    (concat
     (propertize "⟣─ "    'face for-bars)
     (propertize parent   'face for-parent)
     (propertize name     'face for-dir)
     (when branch
       (concat (propertize " ── "    'face for-bars)
               (propertize branch   'face for-git)))

     (propertize "\n"     'face for-bars)
     (propertize (if (= (user-uid) 0) " #" " $") 'face `(:weight ultra-bold))
     ;; (propertize " └→" 'face (if (= (user-uid) 0) `(:weight ultra-bold :foreground "red") `(:weight ultra-bold)))
     (propertize " "    'face `(:weight bold)))))

(setq-default eshell-prompt-function #'eshell/eshell-local-prompt-function)

;; Turn off the default prompt, otherwise, it won’t use ours:

(setq eshell-highlight-prompt nil)

(provide 'cogent-eshell)
