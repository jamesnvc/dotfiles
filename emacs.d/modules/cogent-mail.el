;;; -*- lexical-binding: t -*-

(require 'cogent-keys)

(autoload 'notmuch "notmuch"
  "notmuch mail" t)

;; setup the mail address and use name
(setq mail-user-agent 'message-user-agent
      user-mail-address "james.nvc@gmail.com"
      user-full-name "James N. V. Cash"

      ;; smtp config
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl
      message-send-mail-function 'message-smtpmail-send-it

      ;; report problems with the smtp server
      smtpmail-debug-info t

      ;; add Cc and Bcc headers to the message buffer
      message-default-mail-headers "Cc: \nBcc: \n"

      ;; postponed message is put in the following draft directory
      message-auto-save-directory "~/.mail/gmail/[Gmail]/Drafts"
      message-kill-buffer-on-exit t

      ;; change the directory to store the sent mail
      message-directory "~/.mail/gmail/[Gmail]/Sent Mail")

(with-eval-after-load 'notmuch
  (setq notmuch-address-selection-function
        (lambda (prompt collection initial-input)
          (completing-read prompt (cons initial-input collection) nil t nil
                           'notmuch-address-history)))
  (require 'notmuch-address))

;; from https://gist.github.com/vedang/26a94c459c46e45bc3a9ec935457c80f
(defun cogent/notmuch-search-find-from ()
  "A helper function to find the email address for the given email."
  (let ((notmuch-addr-sexp (first
                            (notmuch-call-notmuch-sexp "address"
                                                       "--format=sexp"
                                                       "--format-version=1"
                                                       "--output=sender"
                                                       (notmuch-search-find-thread-id)))))
    (plist-get notmuch-addr-sexp :address)))

(defun cogent/notmuch-filter-by-from ()
  "Filter the current search view to show all emails sent from the sender of the current thread."
  (interactive)
  (notmuch-search-filter (concat "from:" (cogent/notmuch-search-find-from))))

(defun cogent/notmuch-search-by-from (&optional no-display)
  "Show all emails sent from the sender of the current thread.
NO-DISPLAY is sent forward to `notmuch-search'."
  (interactive)
  (notmuch-search (concat "from:" (cogent/notmuch-search-find-from))
                  notmuch-search-oldest-first
                  nil
                  nil
                  no-display))

(defun cogent/notmuch-tag-by-from (tag-changes &optional beg end refresh)
  "Apply TAG-CHANGES to all emails from the sender of the current thread.
BEG and END provide the region, but are ignored. They are defined
since `notmuch-search-interactive-tag-changes' returns them. If
REFRESH is true, refresh the buffer from which we started the
search."
  (interactive (notmuch-search-interactive-tag-changes))
  (let ((this-buf (current-buffer)))
    (cogent/notmuch-search-by-from t)
    ;; This is a dirty hack since I can't find a way to run a
    ;; temporary hook on `notmuch-search' completion. So instead of
    ;; waiting on the search to complete in the background and then
    ;; making tag-changes on it, I will just sleep for a short amount
    ;; of time. This is generally good enough and works, but is not
    ;; guaranteed to work every time. I'm fine with this.
    (sleep-for 0.5)
    (notmuch-search-tag-all tag-changes)
    (when refresh
      (set-buffer this-buf)
      (notmuch-refresh-this-buffer))))

(add-hook 'message-mode-hook (lambda () (auto-fill-mode -1)))
(add-hook 'message-mode-hook (lambda () (add-to-list 'company-backends 'company-emoji t)))

(general-define-key :keymaps '(notmuch-search-mode-map)
                    "j" #'notmuch-search-next-thread
                    "k" #'notmuch-search-previous-thread
                    "g g" #'notmuch-search-first-thread
                    "G" #'notmuch-search-last-thread
                    "S" #'cogent/notmuch-search-by-from
                    "T" #'cogent/notmuch-tag-by-from)

(general-define-key :keymaps '(notmuch-show-mode-map)
                    "C-c c" #'org-capture)

(use-package helm-notmuch
  :defer t)

(use-package org-mime
  :after (notmuch org)
  :config
  (setq org-mime-export-options
        '(:section-numbers nil :with-author nil :with-toc nil :with-latex dvipng))
  (add-hook 'org-mime-html-hook
            (lambda ()
              (org-mime-change-element-style
               "pre"
               (format "color: %s; background-color: %s; padding: 0.5em;"
                       (face-attribute 'default :foreground)
                       (face-attribute 'default :background)))))
  (add-hook 'org-mime-html-hook
            (lambda ()
              (org-mime-change-element-style
               "blockquote"
               "border-left: 2px solid gray; padding-left: 4px;"))))

(defun cogent/notmuch-inbox ()
  (interactive)
  (notmuch-search "tag:inbox" t))

(use-package calfw
  :commands (cfw:open-calendar-buffer)
  :init
  (defun cogent/calendar ()
    (interactive)
    (unless (boundp 'cogent/gcal-calendar-urls)
      (load-library (concat dotfiles-dir "secrets.el.gpg")))
    (cfw:open-calendar-buffer
     :contents-sources
     (cons
      (cfw:org-create-source "Purple")
      (loop for index from 1
            for url in cogent/gcal-calendar-urls
            collect (cfw:ical-create-source
                     (format "Gcal %d" index)
                     url
                     "DarkGreen")))))
  :config
  (evil-set-initial-state 'cfw:details-mode 'emacs)
  (setq cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓))

(use-package calfw-ical
  :after calfw
  :commands cfw:ical-create-source)

(use-package calfw-org
  :after calfw
  :commands cfw:org-create-source)

(use-package gnus
  :config
  (setq gnus-select-method '(nnnil ""))
  (setq gnus-secondary-select-methods
        '((nntp "news.gmane.io"))))

(provide 'cogent-mail)
