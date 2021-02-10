;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package window-end-visible)

(use-package youtube-dl
  :defer t
  :commands (youtube-dl youtube-dl-list)
  :config
  (evil-set-initial-state 'youtube-dl-list-mode 'emacs)
  :straight (youtube-dl
             :type git
             :host github
             :repo "skeeto/youtube-dl-emacs")
  :custom (youtube-dl-directory "~/Videos/youtube"))

(use-package elfeed
  :commands elfeed
  :config
  (setq-default elfeed-search-filter "@1-week-ago +unread -youtube -news -busy +comic")

  (defface elfeed-comic-face
    '((t :foreground "purple"))
    "Webcomic feed"
    :group 'elfeed)

  (defface elfeed-mustread-face
    '((t :foreground "blue"))
    "Important feed"
    :group 'elfeed)

  (defface elfeed-youtube-face
    '((t :foreground "red"))
    "YouTube feed"
    :group 'elfeed)

  (cl-defun elfeed-dead-feeds (&optional (years 1.0))
    "Return a list of feeds that haven't posted an entry in YEARS years."
    (let* ((living-feeds (make-hash-table :test 'equal))
           (seconds (* years 365.0 24 60 60))
           (threshold (- (float-time) seconds)))
      (with-elfeed-db-visit (entry feed)
        (let ((date (elfeed-entry-date entry)))
          (when (> date threshold)
            (setf (gethash (elfeed-feed-url feed) living-feeds) t))))
      (cl-loop for url in (elfeed-feed-list)
               unless (gethash url living-feeds)
               collect url)))

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

  (defun elfeed-show-youtube-dl ()
    "Download current entry with youtube-dl."
    (interactive)
    (pop-to-buffer (youtube-dl (elfeed-entry-link elfeed-show-entry))))

  (defun elfeed-show-vlc ()
    "Open current entry with VLC."
    (interactive)
    (start-process
     "elfeed-vlc" "*vlc*" (executable-find "vlc")
     (elfeed-entry-link elfeed-show-entry)))

  (cl-defun elfeed-search-youtube-dl (&key slow)
    "Download the current entry with youtube-dl."
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (dolist (entry entries)
        (if (null (youtube-dl (elfeed-entry-link entry)
                              :title (elfeed-entry-title entry)
                              :slow slow))
            (message "Entry is not a YouTube link")
          (message "Downloading %s" (elfeed-entry-title entry)))
        (elfeed-untag entry 'unread)
        (elfeed-search-update-entry entry)
        (unless (use-region-p) (forward-line)))))

  (defalias 'elfeed-search-youtube-dl-slow
    (elfeed-expose #'elfeed-search-youtube-dl :slow t))

  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs)
  :general
  (:keymaps 'elfeed-search-mode-map
            "R" #'cogent/elfeed-mark-visible-read
            "=" #'elfeed-update
            "d" #'elfeed-search-youtube-dl
            "D" #'elfeed-search-youtube-dl-slow
            "L" #'youtube-dl-list
            "l" (lambda () (interactive) (switch-to-buffer (elfeed-log-buffer)))
            "h" (lambda () (interactive) (elfeed-search-set-filter (default-value 'elfeed-search-filter))))
  (:keymaps 'elfeed-show-mode-map
            "o" #'elfeed-show-visit
            "d" #'elfeed-show-youtube-dl
            "V" #'elfeed-show-vlc)
  :custom
  (elfeed-search-face-alist
   '((unread elfeed-search-unread-title-face)
     (mustread elfeed-mustread-face)
     (comic elfeed-comic-face)
     (youtube elfeed-youtube-face)
     (busy shadow))))

(provide 'cogent-feeds)
