;;; -*- lexical-binding: t -*-

(require 'cogent-evil)

(defvar cogent-line-active-bg "#353644"
  "Modeline background colour for active window")

(defvar cogent-line-inactive-bg "#242533"
  "Modeline background colour for inactive window")

(dolist (s '((cogent-line-evil-normal "DarkGoldenrod2" "Evil normal state face.")
             (cogent-line-evil-insert "medium sea green" "Evil insert state face.")
             (cogent-line-evil-emacs "SkyBlue2" "Evil emacs state face.")
             (cogent-line-evil-replace "chocolate" "Evil replace state face.")
             (cogent-line-evil-visual "gray" "Evil visual state face.")
             (cogent-line-evil-motion "plum3" "Evil motion state face.")
             (cogent-line-evil-operator "plum3" "Evil operator state face.")
             (cogent-line-unmodified "DarkGoldenrod2" "Unmodified buffer face.")
             (cogent-line-modified "SkyBlue2" "Modified buffer face.")
             (cogent-line-highlight-face "DarkGoldenrod2" "Default highlight face.")))
  (eval `(defface ,(nth 0 s)
           (list (list t (list :background ,(nth 1 s)
                               :box (list :line-width 4 :color ,(nth 1 s))
                               :foreground ,cogent-line-active-bg)))
           ,(nth 2 s)
           :group 'cogent))
  (eval `(defface ,(intern (s-concat (symbol-name (nth 0 s)) "-inactive"))
           (list (list t (list :foreground ,(nth 1 s)
                               :background nil)))
           ,(nth 2 s)
           :group 'cogent)))

(defvar cogent/evil-state-faces
  '((normal . cogent-line-evil-normal)
    (insert . cogent-line-evil-insert)
    (operator . cogent-line-evil-operator)
    (emacs . cogent-line-evil-emacs)
    (replace . cogent-line-evil-replace)
    (visual . cogent-line-evil-visual)
    (motion . cogent-line-evil-motion)))
(defvar cogent/evil-state-faces-inactive
  '((normal . cogent-line-evil-normal-inactive)
    (insert . cogent-line-evil-insert-inactive)
    (operator . cogent-line-evil-operator-inactive)
    (emacs . cogent-line-evil-emacs-inactive)
    (replace . cogent-line-evil-replace-inactive)
    (visual . cogent-line-evil-visual-inactive)
    (motion . cogent-line-evil-motion-inactive)))

(defun cogent/evil-state-face ()
  (if-let ((face (and
                  (bound-and-true-p evil-local-mode)
                  (assq evil-state
                        (if (cogent-line-selected-window-active-p)
                            cogent/evil-state-faces
                          cogent/evil-state-faces-inactive)))))
      (cdr face)
    cogent-line-default-face))

(defface cogent-line-modified-face
  `((t (:foreground "#8be9fd" :background nil)))
  "Modeline modified-file face"
  :group 'cogent)

(defface cogent-line-modified-face-inactive
  `((t (:foreground "#6272a4" :background nil)))
  "Modeline modified-file face for inactive windows"
  :group 'cogent)

(defface cogent-line-read-only-face
  `((t (:foreground "#ff5555")))
  "Modeline readonly file face.")

(defface cogent-line-read-only-face-inactive
  `((t (:foreground "#aa4949")))
  "Modeline readonly file face for inactive windows.")

;; Keep track of selected window, so we can render the modeline differently
(defvar cogent-line-selected-window (frame-selected-window))
(defun cogent-line-set-selected-window (&rest _args)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq cogent-line-selected-window (frame-selected-window))
    (force-mode-line-update)))
(defun cogent-line-unset-selected-window ()
  (setq cogent-line-selected-window nil)
  (force-mode-line-update))
(add-hook 'window-configuration-change-hook #'cogent-line-set-selected-window)
(add-hook 'focus-in-hook #'cogent-line-set-selected-window)
(add-hook 'focus-out-hook #'cogent-line-unset-selected-window)
(advice-add 'handle-switch-frame :after #'cogent-line-set-selected-window)
(advice-add 'select-window :after #'cogent-line-set-selected-window)
(defun cogent-line-selected-window-active-p ()
  (eq cogent-line-selected-window (selected-window)))

(setq-default mode-line-format
              (list

               '(:eval (propertize (if (eq 'emacs evil-state) "  " "  ")
                                   'face (cogent/evil-state-face)))

               " "
               mode-line-misc-info ; for eyebrowse

               '(:eval (when-let (vc vc-mode)
                         (list " "
                               (propertize (substring vc 5)
                                           'face 'font-lock-comment-face)
                               " ")))

               '(:eval (list
                        ;; the buffer name; the file name as a tool tip
                        (propertize " %b" 'face 'font-lock-type-face
                                    'help-echo (buffer-file-name))
                        (when (buffer-modified-p)
                          (propertize
                           " "
                           'face (if (cogent-line-selected-window-active-p)
                                     'cogent-line-modified-face
                                   'cogent-line-modified-face-inactive)))
                        (when buffer-read-only
                          (propertize
                           ""
                           'face (if (cogent-line-selected-window-active-p)
                                     'cogent-line-read-only-face
                                   'cogent-line-read-only-face-inactive)))
                        " "))

               ;; relative position in file
               '(:eval (list (nyan-create)))
               (propertize "%p" 'face 'font-lock-constant-face)

               ;; spaces to align right
               '(:eval (propertize
                        " " 'display
                        `((space :align-to (- (+ right right-fringe right-margin)
                                              ,(+ 3 (string-width mode-name)))))))

               ;; the current major mode
               (propertize " %m " 'face 'font-lock-string-face)
               ;;minor-mode-alist
               ))

(defun cogent/dracula-mode-line ()
  (set-face-attribute 'mode-line nil
                      :background cogent-line-active-bg
                      :foreground "#f8f8f2"
                      :box `(:line-width 4 :color ,cogent-line-active-bg)
                      :overline nil
                      :underline nil)
  (set-face-attribute 'mode-line-inactive nil
                      :background cogent-line-inactive-bg
                      :foreground "#f8f8f2"
                      :box `(:line-width 4 :color ,cogent-line-inactive-bg)
                      :overline nil
                      :underline nil)
  (with-eval-after-load 'org-faces
    (set-face-attribute 'org-mode-line-clock nil
                        :background nil :inherit nil)))

(provide 'cogent-modeline)
