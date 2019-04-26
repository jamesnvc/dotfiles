;;; -*- lexical-binding: t -*-

(require 'helm)
(require 'helm-lib)
(require 'cl-lib)

;;;###autoload
(defun cogent/eshell-helm ()
  "Switch between or create eshell buffers using helm"
  (interactive)
  (helm :sources (helm-build-sync-source "eshell"
                   :candidates (lambda ()
                                 (->> (buffer-list)
                                     (mapcar
                                      (lambda (buff)
                                        (when (string-prefix-p "*eshell*" (buffer-name buff))
                                          (let ((dir (buffer-local-value 'default-directory buff)))
                                            (cons (pwd-replace-home dir) buff)))))
                                     (remove nil)))
                   :action (lambda (candidate)
                             (message "candidate %s" candidate)
                             (if (bufferp candidate)
                                 (switch-to-buffer candidate)
                               (let ((default-directory candidate))
                                 (eshell t)))))
        :input default-directory
        :buffer "*helm eshell*"))

(provide 'cogent-eshell-helm)
