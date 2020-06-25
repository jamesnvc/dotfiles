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

(featurep 'tab-bar)

(unless (featurep 'tab-bar)
  ;; [TODO] make keybindings for eyebrowse to mimic tab-bar stuff
  (require 'cogent-eyebrowse))

(when (featurep 'tab-bar)
  (general-define-key
   :keymaps 'global
   "C-x t q" #'tab-bar-close-tab
   "C-x t Q" #'tab-bar-close-tab-by-name
   "C-x t t" #'tab-bar-switch-to-recent-tab
   "C-x t c" #'tab-bar-new-tab
   "C-x t C" #'other-tab-prefix
   "C-x t 1" (lambda () (interactive) (tab-bar-select-tab 1))
   "C-x t 2" (lambda () (interactive) (tab-bar-select-tab 2))
   "C-x t 3" (lambda () (interactive) (tab-bar-select-tab 3))
   "C-x t 4" (lambda () (interactive) (tab-bar-select-tab 4))
   "C-x t 5" (lambda () (interactive) (tab-bar-select-tab 5))))

(winner-mode 1)

(provide 'cogent-windows)
