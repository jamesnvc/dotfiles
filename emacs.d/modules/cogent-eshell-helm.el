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
                                        (let ((name (buffer-name buff)))
                                          (when (string-prefix-p "*eshell*" name)
                                            (cons name buff)))))
                                     (remove nil))
                                 ))
        :buffer "*helm eshell*")
  )

(provide 'cogent-eshell-helm)
