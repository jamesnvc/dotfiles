;;; -*- lexical-binding: t -*-

(require 'helm)
(require 'helm-lib)
(require 'cl-lib)
(require 'cogent-helm) ; for vert/horiz switching functions

(defun cogent/buffer-dir-name (buf)
  (pwd-replace-home (buffer-local-value 'default-directory buf)))

(defun cogent/eshell-helm--get-candidates ()
  (let* ((eshells (cl-loop for buf in (buffer-list)
                           when (string-prefix-p "*eshell*" (buffer-name buf))
                           collect (cons (cogent/buffer-dir-name buf) buf) into cands
                           finally return (sort cands (lambda (a b) (string< (car a)
                                                                        (car b))))))
         (new-dir (if (string-blank-p helm-input)
                      default-directory
                    helm-input))
         (new-eshell (cons (concat
                            (propertize
                             " " 'display
                             (propertize "[+]" 'font-lock-face
                                         '(:background "#ff69c6" :foreground "#282a36")))
                            " "
                            (pwd-replace-home new-dir))
                           new-dir)))
    (cons new-eshell eshells)))

(defun cogent/eshell-helm-move-to-first-real-candidate ()
  (let ((sel (helm-get-selection nil nil (helm-get-current-source))))
    (unless (bufferp sel)
      (helm-next-line))))

(defvar helm-cogent-eshell-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-s") #'helm-buffer-switch-horiz-window-command)
    (define-key map (kbd "C-v") #'helm-buffer-switch-vert-window-command)
    map)
  "Keymap for cogent/eshell-helm")

;;;###autoload
(defun cogent/eshell-helm ()
  "Switch between or create eshell buffers using helm"
  (interactive)
  (add-hook 'helm-after-update-hook
            #'cogent/eshell-helm-move-to-first-real-candidate)
  (helm :sources
        (helm-build-sync-source "eshell"
          :keymap helm-cogent-eshell-map
          :candidates #'cogent/eshell-helm--get-candidates
          :action (list
                   (cons
                    "Switch to eshell"
                    (lambda (candidate)
                      (if (bufferp candidate)
                          (switch-to-buffer candidate)
                        (let ((default-directory candidate))
                          (eshell t)))))
                   (cons
                    "Open shell in horizontal split"
                    #'helm-buffer-switch-horiz-window)
                   (cons
                    "Open shell in vertial split"
                    #'helm-buffer-switch-vert-window))
          ;; make the candidates get re-generated on input, so one can
          ;; actually create an eshell in a new directory
          :volatile t
          :cleanup
          (lambda ()
            (remove-hook 'helm-after-update-hook
                         #'cogent/eshell-helm-move-to-first-real-candidate)) )
        :buffer "*helm eshell*"
        :prompt "eshell in: "))

(provide 'cogent-eshell-helm)
