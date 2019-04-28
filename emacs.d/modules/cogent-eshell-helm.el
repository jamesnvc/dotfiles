;;; -*- lexical-binding: t -*-

(require 'helm)
(require 'helm-lib)
(require 'cl-lib)

(defun cogent/eshell-buffers ()
  (cl-loop for buf in (buffer-list)
           when (string-prefix-p "*eshell*" (buffer-name buf))
           collect buf))

;;;###autoload
(defun cogent/eshell-helm ()
  "Switch between or create eshell buffers using helm"
  (interactive)
  (helm :sources
        (helm-build-sync-source "eshell"
          :candidates
          (lambda ()
            (->>
             (cl-loop for buf in (cogent/eshell-buffers)
                      collect (cons (pwd-replace-home
                                     (buffer-local-value
                                      'default-directory buf))
                                    buf))
             (cons
              (let ((new-dir (if (string-blank-p helm-input)
                                 default-directory
                               helm-input)))
                (cons (concat
                       (propertize
                        " " 'display
                        (propertize "[+]" 'font-lock-face
                                    '(:background "#ff69c6" :foreground "#282a36")))
                       " "
                       (pwd-replace-home new-dir))
                      new-dir)))))
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
        :preselect
        (->> (cogent/eshell-buffers)
            first
            (buffer-local-value 'default-directory)
            pwd-replace-home
            regexp-quote
            (concat "^"))
        :buffer "*helm eshell*"
        :prompt "eshell in: "))

(provide 'cogent-eshell-helm)
