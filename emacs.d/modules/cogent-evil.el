;;; -*- lexical-binding: t -*-

(require 'cogent-package)
(require 'cogent-keys)

(use-package evil
  :demand t
  :config
  (defun cogent/evil-yank-to-eol (&optional argument)
    "Yank from point to end of line; like the behaviour I prefer `Y' in
evil to have."
    (interactive "P")
    (let ((beg (point))
          (end (save-excursion
                 (evil-end-of-line)
                 (forward-char)
                 (point))))
      (evil-yank beg end)))

  ;; like vim-unimpaired
  (defun cogent/line-below (&optional argument)
    "New blank line below the current line; like vim-unimpaired."
    (interactive "P")
    (save-excursion
      (dotimes (_ (or argument 1))
        (evil-insert-newline-below))))

  (defun cogent/line-above (&optional argument)
    "New blank line above the current line; like vim-unimpaired."
    (interactive "P")
    (save-excursion
      (dotimes (_ (or argument 1))
        (evil-insert-newline-above))))

  (defun cogent/move-line-up (&optional argument)
    "Move current line up by one or arg"
    (interactive "P")
    (dotimes (_ (or argument 1))
      (transpose-lines 1)
      (previous-logical-line 2)))

  (defun cogent/move-line-down (&optional argument)
    "Move current line down by one or arg"
    (interactive "P")
    (dotimes (_ (or argument 1))
      (next-logical-line 1)
      (transpose-lines 1)
      (previous-logical-line 1)))

  (require 's)

  ;; like vim-abolish
  (defun cogent/change-word-at-point (f)
    (destructuring-bind (start . end) (bounds-of-thing-at-point 'symbol)
      (save-excursion
        (let ((text (buffer-substring-no-properties start end)))
          (while (s-matches? "^[^[:alpha:]]" text)
            (incf start)
            (setf text (buffer-substring-no-properties start end)))
          (delete-region start end)
          (insert (funcall f text))))))

  (defun cogent/evil-interactive-setup ()
    (setq evil-inhibit-operator t)
    (list evil-this-operator))

  (cl-macrolet ((def-abolish-fn (name function)
                  `(defun ,name (operator)
                     (interactive (cogent/evil-interactive-setup))
                     (cl-case operator
                       (evil-change (cogent/change-word-at-point ,function))))))
    (def-abolish-fn cogent/kebab-case #'s-dashed-words)
    (def-abolish-fn cogent/snake-case #'s-snake-case)
    (def-abolish-fn cogent/camel-case #'s-lower-camel-case)
    (def-abolish-fn cogent/camel-case-upper #'s-upper-camel-case))

  :general
  (:keymaps '(normal notmuch-search-mode-map notmuch-hello-mode-map)
   :repeat t
   "C-l" #'evil-window-right
   "C-h" #'evil-window-left
   "C-j" #'evil-window-down
   "C-k" #'evil-window-up
   "<C-tab>" #'evil-window-next
   "<C-S-tab>" #'evil-window-prev
   "<C-S-iso-lefttab>" #'evil-window-prev)
  (:keymaps 'insert
   "C-h" #'evil-delete-backward-char-and-join)
  (:keymaps 'normal
   "j"   #'evil-next-visual-line
   "k"   #'evil-previous-visual-line
   "C-u" #'evil-scroll-up
   "M-u" #'universal-argument)
  (:keymaps '(normal visual)
   "Y"       #'cogent/evil-yank-to-eol
   ;; Like vim-unimpaired
   "[ <SPC>" #'cogent/line-above
   "] <SPC>" #'cogent/line-below
   "[ e"  #'cogent/move-line-up
   "] e"  #'cogent/move-line-down
   ;; Like vim-vinegar
   "-"       #'dired-jump)
  (:states 'operator
   "r-" #'cogent/kebab-case
   "r_" #'cogent/snake-case
   "rc" #'cogent/camel-case
   "rC" #'cogent/camel-case-upper)
  (cogent/leader-def
    :states '(normal visual)
    "w" #'save-buffer
    "<SPC>" #'evil-ex
    "x" #'evil-delete-buffer))

(use-package evil-surround
  :demand t
  :init
  (el-patch-feature evil-surround)
  (el-patch-defvar evil-surround-read-tag-map
    (let ((map (copy-keymap minibuffer-local-map)))
      (define-key map ">" (lambda ()
                            (interactive)
                            (call-interactively 'self-insert-command)
                            (el-patch-swap
                              (run-at-time nil nil
                                           (lambda ()
                                             (when (active-minibuffer-window)
                                               (select-window (active-minibuffer-window))
                                               (exit-minibuffer))))
                              (exit-minibuffer))))
      map)
    "Keymap used by `evil-surround-read-tag'.")
  (el-patch-defun evil-surround-read-from-minibuffer (&rest args)
    (when (el-patch-wrap
            1 1
            (or evil-surround-record-repeat
                (evil-repeat-recording-p)))
      (evil-repeat-keystrokes 'post))
    (let ((res (apply #'read-from-minibuffer args)))
      (when (el-patch-wrap
              1 1
              (or evil-surround-record-repeat
                  (evil-repeat-recording-p)))
        (evil-repeat-record res))
      res))
  :config (global-evil-surround-mode 1))

(use-package evil-search-highlight-persist
  :demand t
  :config
  (defun cogent/evil-remove-search-highlight ()
    (interactive)
    (if (equalp evil-search-module 'evil-search)
        (evil-ex-nohighlight)
      (evil-search-highlight-persist-remove-all)))

  (global-evil-search-highlight-persist t)
  :general
  (general-nmap :prefix "SPC"
    "/" #'cogent/evil-remove-search-highlight))

(use-package evil-nerd-commenter
  :general
  (general-nvmap :prefix "SPC"
    "c SPC" #'evilnc-comment-or-uncomment-lines))

(use-package evil-mc
  :config
  (global-evil-mc-mode 1))

(use-package evil-matchit
  :demand t
  :config (global-evil-matchit-mode 1))

(use-package smooth-scrolling
  :demand t
  :config
  (setq scroll-margin 2
        ;; value > 100 = redisplay won't re-center cursor when going offscreen
        scroll-conservatively 101
        scroll-step 1))

;; for aligning stuff
(use-package evil-lion
  :commands (evil-lion-left evil-lion-right)
  :general
  (:states 'normal
           "g l" #'evil-lion-left
           "g L" #'evil-lion-right))

(use-package origami
  :config (global-origami-mode))

(evil-mode 1)

(provide 'cogent-evil)
