;;; -*- lexical-binding: t -*-

(require 'cogent-package)
(require 'cogent-keys)

(use-package helm
  :config
  (require 'helm-config)
  (require 'helm)
  (helm-mode 1)

  (defmethod helm-setup-user-source ((source helm-source-ffiles))
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

  (helm-autoresize-mode 1)
  (setq-default helm-display-header-line nil
                helm-autoresize-min-height 0
                helm-autoresize-max-height 35
                helm-split-window-inside-p t

                helm-M-x-fuzzy-match t
                helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-match t
                helm-apropos-fuzzy-match t)
  (set-face-attribute 'helm-source-header nil :height 0.75)
  (diminish 'helm-mode)

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
        (let* ((prefix (s-concat "helm-" op-type "-switch-"))
               (vert-split (intern (s-concat prefix "vert-window")))
               (horiz-split (intern (s-concat prefix "horiz-window"))))
          `(progn
             (make-splitter-fn ,vert-split ,open-fn split-window-right)

             (make-splitter-fn ,horiz-split ,open-fn split-window-below)

             (defun ,(intern (s-concat "helm-" op-type "-switch-vert-window-command"))
                 ()
               (interactive)
               (with-helm-alive-p
                 (helm-exit-and-execute-action (quote ,vert-split))))

             (defun ,(intern (s-concat "helm-" op-type "-switch-horiz-window-command"))
                 ()
               (interactive)
               (with-helm-alive-p
                 (helm-exit-and-execute-action (quote ,horiz-split))))))))
    (generate-helm-splitter-funcs "buffer" switch-to-buffer)
    (generate-helm-splitter-funcs "file" find-file)

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
                   t))

    (with-eval-after-load "helm-projectile"
      (helm-add-action-to-source "Display file(s) in new vertical split(s) `C-v'"
                                 #'helm-file-switch-vert-window
                                 helm-source-projectile-files-list)
      (helm-add-action-to-source "Display file(s) in new horizontal split(s) `C-s'"
                                 #'helm-file-switch-horiz-window
                                 helm-source-projectile-files-list)))
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-g" . helm-do-grep)
         ("C-x b" . helm-buffers-list)
         ("C-t" . helm-imenu)
         ("M-y" . helm-show-kill-ring)
         ("<menu>" . helm-M-x))
  :general
  (:keymaps 'helm-buffer-map
   "C-v" #'helm-buffer-switch-vert-window-command
   "C-s" #'helm-buffer-switch-horiz-window-command)
  (:keymaps 'helm-projectile-find-file-map
   "C-v" #'helm-file-switch-vert-window-command
   "C-s" #'helm-file-switch-horiz-window-command)
  (:keymaps 'helm-find-files-map
   "C-v" #'helm-file-switch-vert-window-command
   "C-s" #'helm-file-switch-horiz-window-command)
  (cogent/leader-def
    :states '(normal visual)
    "m" #'helm-M-x
    "T" #'helm-find-files
    "b" #'helm-buffers-list))

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

  (general-def helm-rg-map
    "C-s" #'cogent/helm-rg-switch-horiz-command
    "C-v" #'cogent/helm-rg-switch-vert-command))

(use-package helm-projectile
  :after projectile
  :commands helm-projectile-on helm-projectile-find-file
  :config (projectile-mode)
  :general
  (cogent/leader-def
    :states '(normal visual)
    "P" #'helm-projectile
    "t" #'helm-projectile-find-file
    "s" #'helm-projectile-rg))

(use-package swiper-helm
  :general
  (cogent/leader-def
    :states '(normal visual)
    "l" #'swiper-helm))

(use-package helm-flx
  :config
  (with-eval-after-load "helm"
    (require 'helm-flx)
    (helm-flx-mode 1)))

(use-package helm-ext
  :config
  (helm-ext-ff-enable-skipping-dots t)
  (helm-ext-ff-enable-auto-path-expansion t)
  (helm-ext-minibuffer-enable-header-line-maybe t))

(use-package helm-descbinds
  :commands helm-descbinds-mode
  :init (helm-descbinds-mode))

(provide 'cogent-helm)
