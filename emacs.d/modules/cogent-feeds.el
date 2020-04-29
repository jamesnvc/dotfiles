;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package window-end-visible)

(use-package elfeed
  :commands elfeed
  :hook (elfeed-show-mode . visual-line-mode)
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

  (defun elfeed-search-eww-open (&optional use-generic-p)
    "Open with w3m"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (eww-browse-url it))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

  (defun elfeed-show-eww-open (&optional use-generic-p)
    "Open the current entry in eww"
    (interactive "P")
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link
        (eww-browse-url link))))

  :general
  (general-nmap :keymaps 'elfeed-search-mode-map
    "q" #'quit-window
    "S" #'elfeed-search-set-filter
    "s" #'elfeed-search-live-filter
    "o" #'elfeed-search-browse-url
    "O" #'elfeed-search-eww-open
    "r" #'elfeed-search-untag-all-unread
    "R" #'cogent/elfeed-mark-visible-read
    "y" #'elfeed-search-yank
    "=" #'elfeed-update
    (kbd "RET") #'elfeed-search-show-entry)
  (general-nmap :keymaps 'elfeed-show-mode-map
    "q" #'elfeed-kill-buffer
    "n" #'elfeed-show-next
    "p" #'elfeed-show-prev
    "o" #'elfeed-show-visit
    "O" #'elfeed-show-eww-open))

(use-package elfeed-goodies
  :after elfeed
  :demand t
  :init
  (elfeed-goodies/setup)
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "2 weeks ago"
                                :remove 'unread)))

(provide 'cogent-feeds)
