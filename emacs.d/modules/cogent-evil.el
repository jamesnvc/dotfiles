;;; -*- lexical-binding: t -*-

(require 'cogent-package)
(require 'cogent-keys)

(use-package undo-tree
  :config
  (global-undo-tree-mode 1))

(use-package evil
  :demand t
  :config

  (evil-set-initial-state 'image-mode 'emacs)
  (evil-set-initial-state 'eww-mode 'emacs)
  (evil-set-initial-state 'eww-history-mode 'emacs)
  (evil-set-initial-state 'diff-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'man-mode 'emacs)

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
          (while (string-match-p "^[^[:alpha:]]" text)
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
  :custom
  ((evil-undo-system 'undo-tree))
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
    "x" #'kill-current-buffer
    "X" #'evil-delete-buffer))

(use-package evil-surround
  :demand t
  :init
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

(use-package evil-matchit
  :demand t
  :config (global-evil-matchit-mode 1))

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
