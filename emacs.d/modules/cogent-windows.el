;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package switch-window
  :bind
  (("C-x o" . switch-window)
   ("C-x 1" . switch-window-then-maximize)
   ("C-x 2" . switch-window-then-split-right)
   ("C-x 3" . switch-window-then-split-below)
   ("C-x 0" . switch-window-then-delete))
  :init
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-minibuffer-shortcut ?z)
  (setq switch-window-qwerty-shortcuts
        '("a" "o" "e" "u" "i" "d" "h" "t" "n" "s")))

(provide 'cogent-windows)
