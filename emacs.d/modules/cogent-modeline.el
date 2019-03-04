;;; -*- lexical-binding: t -*-

(require 'cogent-evil)

(dolist (s '((cogent-line-evil-normal "DarkGoldenrod2" "Evil normal state face.")
             (cogent-line-evil-insert "chartreuse3" "Evil insert state face.")
             (cogent-line-evil-emacs "SkyBlue2" "Evil emacs state face.")
             (cogent-line-evil-replace "chocolate" "Evil replace state face.")
             (cogent-line-evil-visual "gray" "Evil visual state face.")
             (cogent-line-evil-motion "plum3" "Evil motion state face.")
             (cogent-line-evil-operator "plum3" "Evil operator state face.")
             (cogent-line-unmodified "DarkGoldenrod2" "Unmodified buffer face.")
             (cogent-line-modified "SkyBlue2" "Modified buffer face.")
             (cogent-line-read-only "plum3" "Read-only buffer face.")
             (cogent-line-highlight-face "DarkGoldenrod2" "Default highlight face.")))
  (eval `(defface ,(nth 0 s)
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

(defun cogent/evil-state-face ()
  (if (bound-and-true-p evil-local-mode)
      ;; nb. spaceline checks if the state is evil-operator state &
      ;; uses evil-previous-state if so
      (let ((face (assq evil-state cogent/evil-state-faces)))
        (if face (cdr face) cogent-line-default-face))
    cogent-line-default-face))

(setq-default mode-line-format
              (list

               '(:eval (propertize " " 'face (cogent/evil-state-face)))

               '(:eval (when-let (vc vc-mode)
                         (propertize (substring vc 5) 'face 'font-lock-comment-face)))

               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize " %b "
                                   'face
                                   (let ((face (buffer-modified-p)))
                                     (if face 'font-lock-warning-face
                                       'font-lock-type-face))
                                   'help-echo (buffer-file-name)))

               ;; line and column
               " (" ;; '%02' to set to 2 chars at least; prevents flickering
               (propertize "%02l" 'face 'font-lock-keyword-face) ","
               (propertize "%02c" 'face 'font-lock-keyword-face)
               ") "

               ;; relative position, size of file
               " ["
               (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
               "/"
               (propertize "%I" 'face 'font-lock-constant-face) ;; size
               "] "

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
                      :background "#353644"
                      :foreground "white"
                      :box '(:line-width 4 :color "#353644")
                      :overline nil
                      :underline nil)
  (set-face-attribute 'mode-line-inactive nil
                      :background "#242533"
                      :foreground "white"
                      :box '(:line-width 4 :color "#242533")
                      :overline nil
                      :underline nil))

(provide 'cogent-modeline)
