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
  (require 'cogent-eyebrowse)
  (winner-mode 1))

(when (featurep 'tab-bar)
  (when (and tab-bar-new-button
             (not (get-text-property 0 'display tab-bar-new-button)))
    (add-text-properties 0 (length tab-bar-new-button)
                         `(display (image :type xpm
                                          :file "tabs/new.xpm"
                                          :margin (2 . 0)
                                          :ascent center))
                         tab-bar-new-button))
  (when (and tab-bar-close-button
             (not (get-text-property 0 'display tab-bar-close-button)))
    (add-text-properties 0 (length tab-bar-close-button)
                         `(display (image :type xpm
                                          :file "tabs/close.xpm"
                                          :margin (2 . 0)
                                          :ascent center))
                         tab-bar-close-button))
  (tab-bar-history-mode) ;; instead of winner-mode
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
   "C-x t 5" (lambda () (interactive) (tab-bar-select-tab 5))
   "C-c <left>" #'tab-bar-history-back
   "C-c <right>" #'tab-bar-history-forward))

(use-package windmove
  :straight (:type built-in)
  :config
  (windmove-default-keybindings))

(use-package window
  :straight (:type built-in)
  :config
  (defun cogent/select-sidebar ()
    (interactive)
    (when-let ((side-win (window-with-parameter 'window-side)))
      (select-window side-win)))
  :custom
  ((display-buffer-alist
        '(;; top side window
          ("\\*\\(Flymake\\|Package-Lint\\|vc-git :\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ("\\*Messages.*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 1)
           (window-parameters . ((no-other-window . t))))
          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 2)
           (window-parameters . ((no-other-window . t))))
          ;; bottom side window
          ("\\*.*Completions.*"
           (display-buffer-in-side-window)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))
          ("\\*cider-error\\*"
           (display-buffer-in-side-window)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))
          ;; left side window
          ("\\*Help.*"
           (display-buffer-in-side-window)
           (window-width . 0.25)
           (side . left)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ;; bottom buffer (NOT side window)
          ("\\*\\vc-\\(incoming\\|outgoing\\).*"
           (display-buffer-at-bottom))
          ("\\*\\(Output\\|Register Preview\\).*"
           (display-buffer-at-bottom)
           (window-parameters . ((no-other-window . t))))))
   (window-combination-resize t)
   (even-window-sizes 'height-only)
   (window-sides-vertical nil)
   (switch-to-buffer-in-dedicated-window 'pop))
  :hook ((help-mode-hook . visual-line-mode)
         (custom-mode-hook . visual-line-mode))
  :bind (("C-x q" . window-toggle-side-windows)
         ("C-x O" . cogent/select-sidebar)))

(provide 'cogent-windows)
