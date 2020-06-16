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

(add-hook 'message-mode-hook (lambda () (auto-fill-mode -1)))
(add-hook 'message-mode-hook (lambda () (add-to-list 'company-backends 'company-emoji t)))

(general-define-key :keymaps '(notmuch-search-mode-map)
                    "j" #'notmuch-search-next-thread
                    "k" #'notmuch-search-previous-thread
                    "g g" #'notmuch-search-first-thread
                    "G" #'notmuch-search-last-thread)

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
                     "Green")))))
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

(provide 'cogent-mail)
