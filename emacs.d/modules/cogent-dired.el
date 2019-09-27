;;; -*- lexical-binding: t -*-

(require 'dired+)
;; Keep dired buffers updated when the file system changes
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq dired-dwim-target t)

(defun delete-current-buffer-file ()
  "Remove the file connected to the current buffer and kills the buffer"
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun rename-current-buffer-file ()
  "Renames the current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(general-def "C-x C-r" #'rename-current-buffer-file)
(general-def :keymaps 'dired-mode-map
  "C-x g" #'magit)
(general-def :states 'normal :keymaps 'dired-mode-map
  "C-l" #'evil-window-right
  "C-h" #'evil-window-left
  "C-j" #'evil-window-down
  "C-k" #'evil-window-up )

(provide 'cogent-dired)
