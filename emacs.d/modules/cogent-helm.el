;;; -*- lexical-binding: t -*-

(require 'cogent-package)
(require 'cogent-keys)

(use-package helm
  :config
  (require 'helm-config)
  (helm-mode 1)
  (helm-autoresize-mode 1)

  ;; Addresses bug (helm says it's in Emacs, Emacs says in helm) that
  ;; gives an error running `describe-keymap' in helm
  (with-eval-after-load 'help-fns
    (defvar keymap-name-history nil))

  (cl-defmethod helm-setup-user-source ((source helm-source-ffiles))
    (helm-source-add-action-to-source-if
     "Magit status"
     (lambda (_candidate)
       (magit-status helm-ff-default-directory))
     source
     (lambda (candidate)
       (and (not (string-match-p ffap-url-regexp candidate))
            helm-ff-default-directory
            (locate-dominating-file helm-ff-default-directory ".git")))
     1))

  (require 'cogent-helm-splits)

  (add-to-list 'helm-type-buffer-actions
               (cons
                "Display buffer(s) in new vertical split(s) `C-v'"
                #'helm-buffer-switch-vert-window)
               t)
  (add-to-list 'helm-type-buffer-actions
               (cons
                "Display buffer(s) in new horizontal split(s) `C-s'"
                #'helm-buffer-switch-horiz-window)
               t)

  (add-to-list 'helm-type-bookmark-actions
               (cons "Display bookmark(s) in new vertical split(s) `C-v'"
                     #'helm-bookmark-switch-vert-window)
               t)
  (add-to-list 'helm-type-bookmark-actions
               (cons
                "Display bookmark(s) in new horizontal split(s) `C-s'"
                #'helm-bookmark-switch-horiz-window)
               t)

  (dolist (list-var '(helm-type-file-actions helm-find-files-actions))
    (add-to-list list-var
                 (cons
                  "Display file(s) in new vertical split(s) `C-v'"
                  #'helm-file-switch-vert-window)
                 t)
    (add-to-list list-var
                 (cons
                  "Display file(s) in new horizontal split(s) `C-s'"
                  #'helm-file-switch-horiz-window)
                 t)) ;; Enable opening helm results in splits

  :custom
  ((helm-ff-refresh-cache-delay 5)
   (helm-ff-keep-cached-candidates nil)

   (helm-display-header-line nil)
   (helm-autoresize-min-height 0)
   (helm-autoresize-max-height 35)

   (helm-buffers-fuzzy-matching t)
   (helm-apropos-fuzzy-match t)

   (helm-show-completion-display-function #'helm-show-completion-default-display-function))

  :custom-face
  (helm-source-header ((t (:height 0.75))))

  :bind (("M-x" . helm-M-x)
         ("<menu>" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         (:map helm-buffer-map
               ("C-v" . helm-buffer-switch-vert-window-command)
               ("C-s" . helm-buffer-switch-horiz-window-command))
         (:map helm-find-files-map
               ("C-v" . helm-file-switch-vert-window-command)
               ("C-s" . helm-file-switch-horiz-window-command)))
  :general
  (cogent/leader-def
    :states '(normal visual)
    "m" #'helm-M-x
    "T" #'helm-find-files
    "b" #'helm-buffers-list
    "l" #'helm-occur))

(use-package helm-bookmarks
  :straight (:type built-in)
  :defer t
  :commands (helm-bookmarks)
  :after (helm)
  :bind (:map helm-bookmark-map
              ("C-v" . helm-bookmark-switch-vert-window-command)
              ("C-s" . helm-bookmark-switch-horiz-window-command)))

(use-package wgrep
  :straight (:type git
                   :host github
                   :repo "mhayashi1120/Emacs-wgrep"))

(use-package helm-rg
  :config
  (defun cogent/switch-to-buffer-split-vert (name)
    (select-window (split-window-right))
    (switch-to-buffer name))
  (defun cogent/switch-to-buffer-split-horiz (name)
    (select-window (split-window-below))
    (switch-to-buffer name))

  (defun cogent/helm-rg-switch-vert (parsed-output &optional highlight-matches)
    (let ((helm-rg-display-buffer-normal-method #'cogent/switch-to-buffer-split-vert))
      (helm-rg--async-action parsed-output highlight-matches)))
  (defun cogent/helm-rg-switch-horiz (parsed-output &optional highlight-matches)
    (let ((helm-rg-display-buffer-normal-method #'cogent/switch-to-buffer-split-horiz))
      (helm-rg--async-action parsed-output highlight-matches)))

  (helm-add-action-to-source
   "Open in horizontal split `C-s'" #'cogent/helm-rg-switch-horiz
   helm-rg-process-source)
  (helm-add-action-to-source
   "Open in vertical split `C-v'" #'cogent/helm-rg-switch-vert
   helm-rg-process-source)

  (defun cogent/helm-rg-switch-vert-command ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action #'cogent/helm-rg-switch-vert)))
  (defun cogent/helm-rg-switch-horiz-command ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action #'cogent/helm-rg-switch-horiz)))

  :bind
  (:map helm-rg-map
        ("C-c C-e" . helm-rg--bounce)
        ("C-s" . cogent/helm-rg-switch-horiz-command)
        ("C-v" . cogent/helm-rg-switch-vert-command))
  :general
  (cogent/leader-def :states '(normal visual)
    "S" #'helm-rg))

(use-package helm-projectile
  :after (projectile helm)
  :commands (helm-projectile-on helm-projectile-find-file)
  :config
  (projectile-mode)
  (helm-add-action-to-source "Display file(s) in new vertical split(s) `C-v'"
                             #'helm-file-switch-vert-window
                             helm-source-projectile-files-list)
  (helm-add-action-to-source "Display file(s) in new horizontal split(s) `C-s'"
                             #'helm-file-switch-horiz-window
                             helm-source-projectile-files-list)
  :bind
  (:map helm-projectile-find-file-map
        ("C-v" . helm-file-switch-vert-window-command)
        ("C-s" . helm-file-switch-horiz-window-command))
  :general
  (cogent/leader-def
    :states '(normal visual)
    "P" #'helm-projectile
    "B" #'helm-projectile-switch-to-buffer
    "t" #'helm-projectile-find-file
    "s" #'helm-projectile-rg))

(use-package helm-descbinds
  :commands helm-descbinds-mode
  :init (helm-descbinds-mode))

(provide 'cogent-helm)
