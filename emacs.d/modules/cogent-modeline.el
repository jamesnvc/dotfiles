;;; -*- lexical-binding: t -*-

(require 'cogent-evil)

(defvar cogent-line-active-bg "#34495e"
  "Modeline background colour for active window")

(defvar cogent-line-inactive-bg "#bfc4ca"
  "Modeline background colour for inactive window")

(defvar cogent-line-evil-state-colours
  '((cogent-line-evil-normal "DarkGoldenrod2" "Evil normal state face.")
    (cogent-line-evil-insert "medium sea green" "Evil insert state face.")
    (cogent-line-evil-emacs "SkyBlue2" "Evil emacs state face.")
    (cogent-line-evil-replace "chocolate" "Evil replace state face.")
    (cogent-line-evil-visual "gray" "Evil visual state face.")
    (cogent-line-evil-motion "plum3" "Evil motion state face.")
    (cogent-line-evil-operator "plum3" "Evil operator state face.")
    (cogent-line-unmodified "DarkGoldenrod2" "Unmodified buffer face.")
    (cogent-line-modified "SkyBlue2" "Modified buffer face.")
    (cogent-line-highlight-face "DarkGoldenrod2" "Default highlight face."))
  "Names, initial colours, and docstring for Evil state modeline indicator")

(dolist (s cogent-line-evil-state-colours)
  (eval `(defface ,(nth 0 s)
           (list (list t (list :foreground ,(nth 1 s)
                               :overline ,cogent-line-active-bg
                               :family "PragmataPro"
                               :background 'unspecified)))
           ,(nth 2 s)
           :group 'cogent))
  (eval `(defface ,(intern (concat (symbol-name (nth 0 s)) "-inactive"))
           (list (list t (list :foreground ,cogent-line-inactive-bg
                               :overline 'unspecified
                               :family "PragmataPro"
                               :background 'unspecified)))
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
  `((t (:foreground "#8be9fd" :background unspecified :family "PragmataPro")))
  "Modeline modified-file face"
  :group 'cogent)

(defface cogent-line-modified-face-inactive
  `((t (:foreground "#6272a4" :background unspecified :family "PragmataPro")))
  "Modeline modified-file face for inactive windows"
  :group 'cogent)

(defface cogent-line-read-only-face
  `((t (:foreground "#ff5555" :family "PragmataPro")))
  "Modeline readonly file face.")

(defface cogent-line-read-only-face-inactive
  `((t (:foreground "#aa4949" :family "PragmataPro")))
  "Modeline readonly file face for inactive windows.")

(defface cogent-line-buffer-name-face
  `((t (:inherit 'font-lock-type-face)))
  "Modeline buffer name face")

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
(add-hook 'window-selection-change-functions #'cogent-line-set-selected-window)
(defun cogent-line-selected-window-active-p ()
  (eq cogent-line-selected-window (selected-window)))

(setopt mode-line-right-align-edge 'window)

(setq-default mode-line-format
              (list

               '(:eval (propertize (if (eq 'emacs evil-state) "  " "  ")
                                   'face (cogent/evil-state-face)))

               " "
               ;; mode-line-misc-info
               ;; '(t erc-modified-channels-object)

               '(:eval (when-let (vc vc-mode)
                         (list " "
                               (propertize (substring vc 5)
                                           'face 'font-lock-comment-face)
                               " ")))

               '(:eval (list
                        ;; the buffer name; the file name as a tool tip
                        " "
                        mode-line-buffer-identification
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

               '(:propertize "%p" 'face 'font-lock-constant-face)


               ;; spaces to align right
               ;; '(:eval (cogent/mode-line-padding))
               'mode-line-format-right-align

               ;; the current major mode
               '(:propertize " %m " 'face 'font-lock-string-face)
               "🌈"
               ;; minor-mode-alist
               ))

(defface cogent-header-project-name
  '((t :foreground "MediumOrchid"))
  "Face for the project name in header line"
  :group 'cogent)

(defface cogent-header-file-path
  '((t :foreground "HotPink"))
  "Face for the file path in header line"
  :group 'cogent)

(defun cogent/project-relative-path ()
  (if-let ((proj (project-current)))
      (concat
       (propertize
        (file-name-nondirectory
         (directory-file-name (project-root proj)))
        'face 'cogent-header-project-name)
       ": "
       (propertize
        (file-relative-name
         (buffer-file-name)
         (project-root (project-current)))
        'face 'cogent-header-file-path))
    (propertize
     (or (buffer-file-name) (concat "👻" (buffer-name)))
     'face 'cogent-header-file-path)))

(defun cogent/header--line-format-right-align ()
  "Like `mode--line-format-right-align' but for header-line-format."
  (let* ((rest (cdr (memq 'cogent/header-line-format-right-align
                          header-line-format)))
         (rest-str (format-mode-line `("" ,@rest)))
         (rest-width (progn
                       (add-face-text-property
                        0 (length rest-str)
                        'header-line t rest-str)
                      (string-pixel-width rest-str))))
    (propertize
     " " 'display
     `(space :align-to (- right-margin (,rest-width))))))

(defvar cogent/header-line-format-right-align
  '(:eval (cogent/header--line-format-right-align))
  "Like `mode-line-format-right-align' but for header-line-format.")
;;;###autoload
(put 'cogent/header-line-format-right-align 'risky-local-variable t)

(setq-default header-line-format
              (list
               '(:eval (cogent/project-relative-path))

               'cogent/header-line-format-right-align
               ;; '(:propertize " " 'display `(space :width 5))

               ;; '(t erc-modified-channels-object)
               ;; 'mode-line-misc-info
               '(which-function-mode
                 (which-func-mode
                  ("" which-func-format "")))
               "🦈"))

(setopt tab-bar-format
        '(;;tab-bar-format-history
          tab-bar-format-tabs-groups
          tab-bar-separator
          ;;tab-bar-format-add-tab
          tab-bar-format-align-right
          tab-bar-format-global
          ))

(defun cogent/hide-completions-header-line ()
  (setq header-line-format nil))
(add-hook 'completion-list-mode-hook #'cogent/hide-completions-header-line)

(comment
 (loop for f being the frames
       for w being the windows of f
       do
       (with-current-buffer (window-buffer w)
         (setq mode-line-format (default-value 'mode-line-format))
         (setq header-line-format (default-value 'header-line-format))
         ))

 (format-mode-line (default-value 'header-line-format))
 )

(provide 'cogent-modeline)
