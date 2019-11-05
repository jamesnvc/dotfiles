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

(use-package helm-notmuch
  :defer t)

(provide 'cogent-mail)
