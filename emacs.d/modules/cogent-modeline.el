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
                               :background nil)))
           ,(nth 2 s)
           :group 'cogent))
  (eval `(defface ,(intern (concat (symbol-name (nth 0 s)) "-inactive"))
           (list (list t (list :foreground ,cogent-line-inactive-bg
                               :overline 'unspecified
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
(advice-add 'select-window :after #'cogent-line-set-selected-window)
(defun cogent-line-selected-window-active-p ()
  (eq cogent-line-selected-window (selected-window)))

(defun cogent/custom-eyebrowse-mode-line-indicator (f &rest args)
  (let ((indicator (apply f args)))
    (if (> (length indicator) 35)
        (let* ((confs (eyebrowse--get 'window-configs))
               (cur (assq (eyebrowse--get 'current-slot) confs))
               (idx (seq-position confs cur))
               (keymap (let ((map (make-sparse-keymap)))
                         (define-key map (kbd "<mode-line><mouse-1>")
                           (lambda (_e)
                             (interactive "e")
                             (eyebrowse-switch-to-window-config
                              (+ 1 (mod (+ idx 1) (length confs))))))
                         map))
               (text (concat
                      (propertize (number-to-string (1+ idx))
                                  'face 'eyebrowse-mode-line-active)
                      (propertize "/"
                                  'face 'eyebrowse-mode-line-separator)
                      (propertize (number-to-string (length confs))
                                  'face 'eyebrowse-mode-line-inactive))))
          (concat
           (propertize eyebrowse-mode-line-left-delimiter
                       'face 'eyebrowse-mode-line-delimiters)
           (propertize text
                       'local-map keymap
                       'help-echo "Next workspace")
           (propertize eyebrowse-mode-line-right-delimiter
                       'face 'eyebrowse-mode-line-delimiters)))
      indicator)))
(advice-add 'eyebrowse-mode-line-indicator
            :around #'cogent/custom-eyebrowse-mode-line-indicator)

(setq-default mode-line-format
              (list

               '(:eval (propertize (if (eq 'emacs evil-state) "  " "  ")
                                   'face (cogent/evil-state-face)))

               " "
               mode-line-misc-info ; for eyebrowse
               '(t erc-modified-channels-object)

               '(:eval (when-let (vc vc-mode)
                         (list " "
                               (propertize (substring vc 5)
                                           'face 'font-lock-comment-face)
                               " ")))

               '(:eval (list
                        ;; the buffer name; the file name as a tool tip
                        " "
                        (propertize "%b" 'face 'cogent-line-buffer-name-face
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

               '(:propertize "%p" 'face 'font-lock-constant-face)
               '(pdf-misc-size-indication-minor-mode
                 (:eval (let* ((page (pdf-view-current-page))
                               (pdf-page (nth (1- page) (pdf-cache-pagelabels))))
                          (list
                           " "
                           (when (not (string= (number-to-string page) pdf-page))
                             (list "(" pdf-page ") "))
                           (number-to-string (pdf-view-current-page))
                           "/"
                           (number-to-string (pdf-cache-number-of-pages))))))

               ;; spaces to align right
               '(:eval (propertize
                        " " 'display
                        `((space :align-to (- (+ right right-fringe right-margin)
                                              ,(+ 3 (string-width
                                                     (if (listp mode-name)
                                                         (car mode-name)
                                                       mode-name))))))))

               ;; the current major mode
               '(:propertize " %m " 'face 'font-lock-string-face)
               ;;minor-mode-alist
               ))

(provide 'cogent-modeline)
