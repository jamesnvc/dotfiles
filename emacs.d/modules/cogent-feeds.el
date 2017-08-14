;;; -*- lexical-binding: t -*-

(use-package elfeed
  :commands elfeed
  :init
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "2 weeks ago"
                                :remove 'unread))
  (evil-define-key 'normal elfeed-search-mode-map
    "q" #'quit-window
    "S" #'elfeed-search-set-filter
    "s" #'elfeed-search-live-filter
    "o" #'elfeed-search-browse-url
    "r" #'elfeed-search-untag-all-unread
    "y" #'elfeed-search-yank
    "R" #'elfeed-update
    (kbd "RET") #'elfeed-search-show-entry)
  (evil-define-key 'normal elfeed-show-mode-map
    "q" #'elfeed-kill-buffer
    "n" #'elfeed-show-next
    "p" #'elfeed-show-prev
    "o" #'elfeed-show-visit))

(provide 'cogent-feeds)
