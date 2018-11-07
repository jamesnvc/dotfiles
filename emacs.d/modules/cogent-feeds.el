;;; -*- lexical-binding: t -*-

(use-package elfeed
  :commands elfeed
  :init
  (add-hook 'elfeed-show-mode-hook #'visual-line-mode)
  (evil-define-key 'normal elfeed-search-mode-map
    "q" #'quit-window
    "S" #'elfeed-search-set-filter
    "s" #'elfeed-search-live-filter
    "o" #'elfeed-search-browse-url
    "r" #'elfeed-search-untag-all-unread
    "y" #'elfeed-search-yank
    "=" #'elfeed-update
    (kbd "RET") #'elfeed-search-show-entry)
  (evil-define-key 'normal elfeed-show-mode-map
    "q" #'elfeed-kill-buffer
    "n" #'elfeed-show-next
    "p" #'elfeed-show-prev
    "o" #'elfeed-show-visit))

(use-package elfeed-goodies
  :after elfeed
  :demand t
  :init
  (elfeed-goodies/setup)
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "2 weeks ago"
                                :remove 'unread)))

(provide 'cogent-feeds)
