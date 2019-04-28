;;; -*- lexical-binding: t -*-

(require 'helm)
(require 'helm-lib)
(require 'cl-lib)

;;;###autoload
(defun cogent/eshell-helm ()
  "Switch between or create eshell buffers using helm"
  (interactive)
  (helm :sources
        (helm-build-sync-source "eshell"
          :candidates (lambda ()
                        (->> (buffer-list)
                            (mapcar
                             (lambda (buff)
                               (when (string-prefix-p "*eshell*" (buffer-name buff))
                                 (let ((dir (buffer-local-value 'default-directory buff)))
                                   (cons (pwd-replace-home dir) buff)))))
                            (cons
                             (let ((new-dir (if (string-blank-p helm-input) default-directory helm-input)))
                               (cons (s-concat "new " (pwd-replace-home new-dir))
                                     new-dir)))
                            (remove nil)))
          ;; [TODO] make completing-read function be directory
          :action (lambda (candidate)
                    (message "candidate %s" candidate)
                    (if (bufferp candidate)
                        (switch-to-buffer candidate)
                      (let ((default-directory candidate))
                        (eshell t))))
          :volatile t)
        :buffer "*helm eshell*"))

(provide 'cogent-eshell-helm)
