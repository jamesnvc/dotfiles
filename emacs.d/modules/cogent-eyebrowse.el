;;; -*- lexical-binding: t -*-

(use-package eyebrowse
  :commands eyebrowse-mode
  :init (eyebrowse-mode t)
  :config

  (defun cogent/eyebrowse-get-candidates ()
    (thread-last (mapcar (lambda (it) (cons (eyebrowse-format-slot it) (car it)))
                         (eyebrowse--get 'window-configs))
                 (cons (cons
                        (concat
                         (propertize
                          " " 'display
                          (propertize "[+]" 'font-lock-face
                                      '(:background "#ff69c6" :foreground "#282a36")))
                         " " helm-input)
                        helm-input))))

  (defun cogent/eyebrowse-helm ()
    "Manage eyebrowse window configs"
    (interactive)
    (add-hook 'helm-after-update-hook
              #'helm-next-line)
    (helm :sources
          (helm-build-sync-source "eyebrowse"
                                  :volatile t
                                  :candidates #'cogent/eyebrowse-get-candidates
                                  :action (list
                                           (cons "Switch to config"
                                                 (lambda (candidate)
                                                   (if (stringp candidate)
                                                       (progn
                                                         (eyebrowse-create-window-config)
                                                         (eyebrowse-rename-window-config
                                                          (eyebrowse--get 'current-slot)
                                                          candidate))
                                                     (eyebrowse-switch-to-window-config candidate))))
                                           (cons "Close config"
                                                 (lambda (candidate)
                                                   (unless (stringp candidate)
                                                     (let ((window-configs (eyebrowse--get 'window-configs))
                                                           (current (eyebrowse--get 'current-slot)))
                                                       (when (> (length window-configs) 1)
                                                         (when (= candidate current)
                                                           (if (equal (assq current window-configs)
                                                                      (car (last window-configs)))
                                                               (eyebrowse-prev-window-config nil)
                                                             (eyebrowse-next-window-config nil)))
                                                         (eyebrowse--delete-window-config candidate))))))
                                           (cons "Rename config"
                                                 (lambda (candidate)
                                                   (unless (stringp candidate)
                                                     (eyebrowse-rename-window-config
                                                      candidate
                                                      (read-string "New tag: "))))))
                                  :cleanup (lambda ()
                                             (remove-hook 'helm-after-update-hook
                                                          #'helm-next-line)))
          :buffer "*helm eyebrowse*"
          :prompt "eyebrowse: "))

  :config
  ((eyebrowse-mode-line-left-delimiter "❲")
   (eyebrowse-mode-line-right-delimiter "❳")
   (eyebrowse-mode-line-separator " ")
   (eyebrowse-new-workspace t)))

(provide 'cogent-eyebrowse)
