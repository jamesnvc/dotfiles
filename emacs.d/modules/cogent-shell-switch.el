;;; -*- lexical-binding: t -*-

(defun cogent-shell--pwd-replace-home (directory)
  "Replace $HOME in DIRECTORY with tilde character."
  (let ((home (expand-file-name (getenv "HOME"))))
    (if (string-prefix-p home directory)
        (concat "~" (substring directory (length home)))
      directory)))

(defun cogent-shell--buffer-shell-indicator (buf)
  "Display an indicator as to the type of shell in BUF."
  (thread-last buf
    (buffer-local-value 'major-mode)
    symbol-name string-to-char upcase string))

(defun cogent-shell--candidate-name (buf)
  "Display the directory of BUF, with HOME replaced with tilde."
  (let ((buffer-mode (buffer-local-value 'major-mode buf)))
    (when (cl-member buffer-mode '("eshell-mode" "shell-mode" "vterm-mode")
                     :test #'string=)
      (list
       (cons 'buffer-name (buffer-name buf))
       (cons 'path (buffer-local-value 'default-directory buf))))))

(defun cogent/all-shell-candidates ()
  (let* ((here (expand-file-name default-directory))
         (dist2here (lambda (d)
                      (let* ((prefix (compare-strings
                                      here 0 nil
                                      (expand-file-name d) 0 nil))
                             (prefix-len (if (numberp prefix)
                                             (- (abs prefix) 1)
                                           (length d))))
                        (thread-first (substring here 0 prefix-len)
                          (split-string "/")
                          (length)
                          (+ (if (numberp prefix) 0 2))))))
         (shells (cl-loop for buf being the buffers
                          when (cogent-shell--candidate-name buf)
                          collect (cons it buf) into cands
                          finally return (thread-first cands
                                           (sort (lambda (a b)
                                                   (< (length (alist-get 'path (car a)))
                                                      (length (alist-get 'path (car b))))))
                                           (sort
                                            (lambda (a b)
                                              (> (funcall dist2here (alist-get 'path (car a)))
                                                 (funcall dist2here (alist-get 'path (car b)))))))))
         (candidates (cl-loop for cand-buf in shells
                              for cand = (car cand-buf)
                              for buf = (cdr cand-buf)
                              collect (cons (alist-get 'buffer-name cand)
                                            buf)
                              into cands
                              finally return cands)))
    (append candidates (list (cons here nil)))))

(defun cogent/switch-shell ()
  (interactive)
  (let* ((shells (cogent/all-shell-candidates))
         (choice (completing-read
                  "Shell: "
                  (lambda (input predicate action)
                    (if (eq action 'metadata)
                        '(metadata
                          (category . shell)
                          ;; we're already sorting
                          (display-sort-function . identity)
                          (cycle-sort-function . identity))
                      (complete-with-action action shells input predicate))))))
    (if-let ((sel (cdr (assoc choice shells))))
        (switch-to-buffer sel)
      (let ((default-directory choice))
        (eshell t)))))

(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-annotator-registry
               '(shell cogent-shell--annotator none)))

(defun cogent-shell--annotator (cand)
  "Annotate shell buffer CAND with shell type and path"
  (when-let (buffer (get-buffer cand))
    (marginalia--fields
     ((cogent-shell--buffer-shell-indicator buffer)
      :face 'marginalia-mode)
     ((cogent-shell--pwd-replace-home
       (buffer-local-value 'default-directory buffer))
      :face 'marginalia-documentation
      :truncate marginalia-truncate-width))))

(defun cogent-shell--switch-to-eshell (cand)
  (interactive "s")
  (let ((path (if-let (buf (get-buffer cand))
                  (buffer-local-value 'default-directory buf)
                cand)))
    (let ((default-directory path))
      (eshell t))))

(defun cogent-shell--switch-to-vterm (cand)
  (interactive "s")
  (let* ((path (if-let (buf (get-buffer cand))
                   (buffer-local-value 'default-directory buf)
                 cand))
         (default-directory path))
    (vterm t)))

(defun cogent-shell--switch-horiz-split (cand)
  (interactive "s")
  (if-let (buffer (get-buffer cand))
      (cogent--split-below #'switch-to-buffer buffer)
    (progn
      (select-window (split-window-below))
      (let ((default-directory cand))
        (eshell t)))))
(defun cogent-shell--switch-vert-split (cand)
  (interactive "s")
  (if-let (buffer (get-buffer cand))
      (cogent--split-right #'switch-to-buffer buffer)
    (progn
      (select-window (split-window-right))
      (let ((default-directory cand))
        (eshell t)))))

(with-eval-after-load 'embark

  (embark-define-keymap embark-shell-actions
    "Keymap for actions for shell buffers"
    ("RET" switch-to-buffer)
    ("e" cogent-shell--switch-to-eshell)
    ("v" cogent-shell--switch-to-vterm)
    ("C-s" cogent-shell--switch-horiz-split)
    ("C-v" cogent-shell--switch-vert-split))

  (add-to-list 'embark-keymap-alist '(shell . embark-shell-actions)))

(provide 'cogent-shell-switch)
