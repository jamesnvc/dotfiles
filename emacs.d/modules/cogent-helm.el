;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package helm
  :config
  (require 'helm-config)
  (require 'helm)
  (helm-mode 1)
  (with-eval-after-load "cogent-project"
    (use-package helm-projectile
      :commands helm-projectile-on helm-projectile-find-file
      :config (projectile-mode)))
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
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-g" . helm-do-grep)
         ("C-x b" . helm-buffers-list)
         ("C-t" . helm-imenu)
         ("M-y" . helm-show-kill-ring)))

(use-package swiper-helm
  :bind (("C-S-s" . swiper-helm)))

(use-package helm-flx
  :config
  (with-eval-after-load "helm"
    (require 'helm-flx)
    (helm-flx-mode 1)))

(use-package helm-ag)

(use-package helm-ext
  :config
  (helm-ext-ff-enable-skipping-dots t)
  (helm-ext-ff-enable-auto-path-expansion t))

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

(defun cogent/make-find-file-in-split (split-fn)
  (lambda (f)
    (select-window (funcall split-fn))
    (find-file f)))

(defun cogent/helm-ag-switch-new-horiz-window ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action
     (lambda (candidate)
       (helm-ag--find-file-action
        candidate
        (cogent/make-find-file-in-split #'split-window-below)
        (helm-ag--search-this-file-p))))))

(defun cogent/helm-ag-switch-new-vert-window ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action
     (lambda (candidate)
       (helm-ag--find-file-action
        candidate
        (cogent/make-find-file-in-split #'split-window-right)
        (helm-ag--search-this-file-p))))))

(with-eval-after-load 'helm-ag
  (add-to-list 'helm-ag--actions
               (cons "Display file in new horizontal split"
                     #'cogent/helm-ag-switch-new-horiz-window))
  (add-to-list 'helm-ag--actions
               (cons "Display file in new vertical split"
                     #'cogent/helm-ag-switch-new-vert-window)))

(general-def helm-buffer-map
  "C-v" #'helm-buffer-switch-new-vert-window
  "C-s" #'helm-buffer-switch-new-horiz-window)
(general-def helm-projectile-find-file-map
  "C-v" #'helm-file-switch-new-vert-window
  "C-s" #'helm-file-switch-new-horiz-window)
(general-def helm-find-files-map
  "C-v" #'helm-file-switch-new-vert-window
  "C-s" #'helm-file-switch-new-horiz-window)
(general-def helm-ag-map
  "C-s" #'cogent/helm-ag-switch-new-horiz-window
  "C-v" #'cogent/helm-ag-switch-new-vert-window)

(provide 'cogent-helm)
