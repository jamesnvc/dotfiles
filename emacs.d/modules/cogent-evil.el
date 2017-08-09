;;; -*- lexical-binding: t -*-

(use-package evil
  :config
  (use-package evil-surround
    :demand t
    :config (global-evil-surround-mode 1))
  (use-package evil-leader
    :config
    (setq evil-leader/in-all-states 1)
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>"))
  (use-package evil-search-highlight-persist
    :demand t
    :config
    (global-evil-search-highlight-persist t)
    (evil-leader/set-key "/" 'evil-search-highlight-persist-remove-all))
  (use-package evil-nerd-commenter
    :config
    (evil-leader/set-key
      "c SPC" 'evilnc-comment-or-uncomment-lines))
  (use-package evil-mc
    :config
    (global-evil-mc-mode 1))
  (use-package smooth-scrolling
    :demand t
    :config
    (setq scroll-margin 2
          ;; value > 100 = redisplay won't re-center cursor when going offscreen
          scroll-conservatively 101
          scroll-step 1)))

(evil-mode 1)

(provide 'cogent-evil)
