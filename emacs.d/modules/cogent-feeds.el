;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package window-end-visible)

(use-package elfeed
  :commands elfeed
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

  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs)
  :general
  (:keymaps 'elfeed-search-mode-map
            "R" #'cogent/elfeed-mark-visible-read
            "=" #'elfeed-update)
  (:keymaps 'elfeed-show-mode-map
            "o" #'elfeed-show-visit))

(provide 'cogent-feeds)
