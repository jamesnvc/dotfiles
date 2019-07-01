;;; -*- lexical-binding: t -*-

(require 'helm)
(require 'helm-lib)
(require 'cl-lib)
(require 'cogent-helm) ; for vert/horiz switching functions

(defun cogent/buffer-dir-name (buf)
  (pwd-replace-home (buffer-local-value 'default-directory buf)))

(defun cogent/eshell-helm--get-candidates ()
  (let* ((here (expand-file-name default-directory))
         (dist2here (lambda (d)
                      (let ((prefix (compare-strings
                                     here 0 nil
                                     (expand-file-name d) 0 nil)))
                        (->> (if (numberp prefix)
                                (- (abs prefix) 1)
                              (length d))
                            (substring here 0)
                            (s-split "/")
                            length
                            (+ (if (numberp prefix) 0 2))))))
         (eshells (cl-loop for buf in (buffer-list)
                           when (string-prefix-p "*eshell*" (buffer-name buf))
                           collect (cons (cogent/buffer-dir-name buf) buf) into cands
                           finally return (-> cands
                                              (sort (lambda (a b) (< (length (car a)) (length (car b)))))
                                              (sort
                                               (lambda (a b) (> (funcall dist2here (car a))
                                                           (funcall dist2here (car b))))))))
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

(defun cogent/eshell-helm-horiz-split (candidate)
  (if (bufferp candidate)
      (helm-buffer-switch-horiz-window candidate)
    (let ((default-directory candidate))
      (select-window (split-window-below))
      (eshell t)
      (balance-windows))))

(defun cogent/eshell-helm-vert-split (candidate)
  (if (bufferp candidate)
      (helm-buffer-switch-vert-window candidate)
    (let ((default-directory candidate))
      (select-window (split-window-right))
      (eshell t)
      (balance-windows))))

(defun cogent/eshell-helm-horiz-split-command ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'cogent/eshell-helm-horiz-split)))

(defun cogent/eshell-helm-vert-split-command ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'cogent/eshell-helm-vert-split)))

(defvar helm-cogent-eshell-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-s") #'cogent/eshell-helm-horiz-split-command)
    (define-key map (kbd "C-v") #'cogent/eshell-helm-vert-split-command)
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
                    #'cogent/eshell-helm-horiz-split)
                   (cons
                    "Open shell in vertical split"
                    #'cogent/eshell-helm-vert-split))
          ;; make the candidates get re-generated on input, so one can
          ;; actually create an eshell in a new directory
          :volatile t
          :cleanup
          (lambda ()
            (remove-hook 'helm-after-update-hook
                         #'cogent/eshell-helm-move-to-first-real-candidate)))
        :buffer "*helm eshell*"
        :prompt "eshell in: "))

(provide 'cogent-eshell-helm)
