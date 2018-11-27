;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package window-end-visible)

(use-package elfeed
  :commands elfeed
  :init
  (add-hook 'elfeed-show-mode-hook #'visual-line-mode)
  :config
  (defun cogent/elfeed-mark-visible-read ()
    "From http://xenodium.com/#faster-elfeed-browsing-with-paging"
    (interactive)
    (require 'window-end-visible)
    (set-mark (window-start))
    (goto-char (window-end-visible))
    (activate-mark)
    (elfeed-search-untag-all-unread)
    (elfeed-search-update--force)
    (deactivate-mark)
    (goto-char (window-start)))
  :general
  (general-nmap :keymaps 'elfeed-search-mode-map
    "q" #'quit-window
    "S" #'elfeed-search-set-filter
    "s" #'elfeed-search-live-filter
    "o" #'elfeed-search-browse-url
    "r" #'elfeed-search-untag-all-unread
    "R" #'cogent/elfeed-mark-visible-read
    "y" #'elfeed-search-yank
    "=" #'elfeed-update
    (kbd "RET") #'elfeed-search-show-entry)
  (general-nmap :keymaps 'elfeed-show-mode-map
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
