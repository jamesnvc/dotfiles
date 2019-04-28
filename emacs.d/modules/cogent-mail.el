;;; -*- lexical-binding: t -*-

(require 'cogent-keys)

(autoload 'notmuch "notmuch"
  "notmuch mail" t)


;; setup the mail address and use name
(setq mail-user-agent 'message-user-agent
      user-mail-address (cogent/user-email)
      user-full-name (cogent/user-full-name)

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
      message-auto-save-directory "~/mail/draft"
      message-kill-buffer-on-exit t

      ;; change the directory to store the sent mail
      message-directory "~/mail/")

(with-eval-after-load "notmuch"
  (setq notmuch-address-selection-function
        (lambda (prompt collection initial-input)
          (completing-read prompt (cons initial-input collection) nil t nil
                           'notmuch-address-history)))
  (require 'notmuch-address)
  (notmuch-address-message-insinuate))

(defun cogent/sync-mail ()
  "Run mail syncing shell script."
  (interactive)
  (let ((process-connection-type nil))
    (start-process "sync-mail-process" nil
                   (expand-file-name "~/bin/sync_mail.sh"))))

(general-define-key :keymaps '(notmuch-search-mode-map)
                    "j" #'notmuch-search-next-thread
                    "k" #'notmuch-search-previous-thread
                    "g g" #'notmuch-search-first-thread
                    "G" #'notmuch-search-last-thread)
(general-def :keymaps '(notmuch-hello-mode-map
                        notmuch-search-mode-map)
  "!" #'cogent/sync-mail)

(use-package helm-notmuch)

(provide 'cogent-mail)
