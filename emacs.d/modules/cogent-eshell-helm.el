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
          :candidates
          (lambda ()
            (-> (cl-loop for buf in (buffer-list)
                     when (string-prefix-p "*eshell*" (buffer-name buf))
                     collect (cons (pwd-replace-home
                                    (buffer-local-value
                                     'default-directory buf))
                                   buf))
                (append
                 (let ((new-dir (if (string-blank-p helm-input)
                                    default-directory
                                  helm-input)))
                   (list
                    (cons (s-concat "new " (pwd-replace-home new-dir))
                         new-dir))))))
          :action (list
                   (cons
                    "Switch to eshell"
                    (lambda (candidate)
                      (if (bufferp candidate)
                          (switch-to-buffer candidate)
                        (let ((default-directory candidate))
                          (eshell t))))))
          ;; make the candidates get re-generated on input, so one can
          ;; actually create an eshell in a new directory
          :volatile t)
        :buffer "*helm eshell*"))

(provide 'cogent-eshell-helm)
