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

(use-package dwim-shell-command
  :straight (dwim-shell-command
             :type git
             :host github
             :branch "main"
             :repo "xenodium/dwim-shell-command")

  :config

  (defun cogent/dwim-convert-wav-to-caf ()
    "Convert marked wav files to cafs"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert to caf"
     "afconvert -f caff -d LEI16@44100 -c 1 <<f>> <<fne>>.caf"
     :utils "afconvert"))

  (defun cogent/dwim-shell-command-convert-image-to-icns ()
    "Convert png to icns icon."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert png to icns icon"
     "
    # Based on http://stackoverflow.com/questions/12306223/how-to-manually-create-icns-files-using-iconutil
    # Note: png must be 1024x1024
    mkdir <<fne>>.iconset
    sips -z 16 16 '<<f>>' --out '<<fne>>.iconset/icon_16x16.png'
    sips -z 32 32 '<<f>>' --out '<<fne>>.iconset/icon_16x16@2x.png'
    sips -z 32 32 '<<f>>' --out '<<fne>>.iconset/icon_32x32.png'
    sips -z 64 64 '<<f>>' --out '<<fne>>.iconset/icon_32x32@2x.png'
    sips -z 128 128 '<<f>>' --out '<<fne>>.iconset/icon_128x128.png'
    sips -z 256 256 '<<f>>' --out '<<fne>>.iconset/icon_128x128@2x.png'
    sips -z 256 256 '<<f>>' --out '<<fne>>.iconset/icon_256x256@2x.png'
    sips -z 512 512 '<<f>>' --out '<<fne>>.iconset/icon_512x512.png'
    sips -z 512 512 '<<f>>' --out '<<fne>>.iconset/icon_256x256@2x.png'
    sips -z 1024 1024 '<<f>>' --out '<<fne>>.iconset/icon_512x512@2x.png'
    iconutil -c icns '<<fne>>.iconset'"
     :utils '("sips" "iconutil")
     :extensions "png"))

  (defun cogent/dwim-shell-command-convert-image-to-watch-icns ()
    "Convert png to icns icon."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert png to icns icon"
     "
    # Note: png must be 1024x1024
    mkdir <<fne>>.iconset
    sips -z 48 48 '<<f>>' --out '<<fne>>.iconset/icon_24x24@2x.png'
    sips -z 55 55 '<<f>>' --out '<<fne>>.iconset/icon_27x27@2x.png'
    sips -z 66 66 '<<f>>' --out '<<fne>>.iconset/icon_33x33@2x.png'
    sips -z 58 58 '<<f>>' --out '<<fne>>.iconset/icon_29x29@2x.png'
    sips -z 87 87 '<<f>>' --out '<<fne>>.iconset/icon_43x43@2x.png'
    sips -z 80 80 '<<f>>' --out '<<fne>>.iconset/icon_40x40@2x.png'
    sips -z 88 88 '<<f>>' --out '<<fne>>.iconset/icon_44x44@2x.png'
    sips -z 92 92 '<<f>>' --out '<<fne>>.iconset/icon_46x46@2x.png'
    sips -z 100 100 '<<f>>' --out '<<fne>>.iconset/icon_50x50@2x.png'
    sips -z 102 102 '<<f>>' --out '<<fne>>.iconset/icon_51x51@2x.png'
    sips -z 172 172 '<<f>>' --out '<<fne>>.iconset/icon_86x86@2x.png'
    sips -z 196 196 '<<f>>' --out '<<fne>>.iconset/icon_98x98@2x.png'
    sips -z 216 216 '<<f>>' --out '<<fne>>.iconset/icon_108x108@2x.png'
    sips -z 234 234 '<<f>>' --out '<<fne>>.iconset/icon_117x117@2x.png'
    sips -z 1024 1024 '<<f>>' --out '<<fne>>.iconset/icon_1024x1024.png'
    iconutil -c icns '<<fne>>.iconset'"
     :utils '("sips" "iconutil")
     :extensions "png"))

  (require 'cl-lib)

  (defun cogent/dwim-yt-dl ()
    "Download video url from clipboard"
    (interactive)
    (cl-assert (string-match-p "^http[s]?://" (current-kill 0)) nil "Not a URL")
    (dwim-shell-command-on-marked-files
     "Downloading"
     "yt-dlp --newline -o \"~/Movies/youtube/%(title)s.%(ext)s\" \"<<cb>>\""
     :utils "yt-dlp"
     :no-progress t
     :error-autofocus t
     :monitor-directory "~/Movies/youtube"
     :silent-success t))

  (defun cogent/dwim-mov-to-gif ()
    "Convert a video file to a gif via ffmpeg"
    (interactive)
    (dwim-shell-command-on-marked-files
     "ffmpeg"
     "ffmpeg -i '<<f>>' -vf \"fps=10,scale=640:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse\" -loop 0 '<<fne>>.gif'"
     :utils '("ffmpeg")))

  (define-key global-map (kbd "M-!") #'dwim-shell-command)
  (define-key dired-mode-map (kbd "&") #'dwim-shell-command)
  (define-key dired-mode-map (kbd "M-&") #'dired-do-async-shell-command))

(require 'cogent-shell-switch)

(provide 'cogent-shell)
