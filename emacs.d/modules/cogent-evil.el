;;; -*- lexical-binding: t -*-

(require 'cogent-package)
(require 'cogent-keys)

(use-package evil
  :demand t
  :general
  (:keymaps 'insert
            "C-h" #'evil-delete-backward-char-and-join)
  (:keymaps 'normal
            "j" #'evil-next-visual-line
            "k" #'evil-previous-visual-line
            "C-u" #'evil-scroll-up
            "M-u" #'universal-argument)
  (cogent/leader-def
   :states '(normal visual)
   "<SPC>" #'evil-ex
   "x" #'evil-delete-buffer))

(use-package evil-surround
  :demand t
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

(evil-mode 1)

(provide 'cogent-evil)
