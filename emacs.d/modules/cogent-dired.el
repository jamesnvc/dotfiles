;;; -*- lexical-binding: t -*-

;; Keep dired buffers updated when the file system changes
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(use-package dired
  :straight (:type built-in)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setopt dired-mouse-drag-files t)
  :custom
  ((dired-dwim-target t)
   (dired-recursive-copies 'always)
   (dired-recursive-deletes 'always)
   (dired-listing-switches
    "-AGFhlv --group-directories-first --time-style=long-iso"))
  :hook
  ((dired-mode-hook . dired-hide-details-mode)
   (dired-mode-hook . hl-line-mode))
  :bind
  (:map dired-mode-map
        ("C-x g" . magit))
  :general
  (:states 'normal :keymaps 'dired-mode-map
           "C-l" #'evil-window-right
           "C-h" #'evil-window-left
           "C-j" #'evil-window-down
           "C-k" #'evil-window-up))

(use-package dired-aux
  :straight (:type built-in)
  :custom
  ((dired-isearch-filenames 'dwim)
   (dired-create-destination-dirs 'ask)))

(use-package dired-git-info
  :bind (:map dired-mode-map (")" . dired-git-info-mode)))

(use-package wdired
  :straight (:type built-in)
  :after dired
  :commands wdired-change-to-wdired-mode
  :custom
  ((wdired-allow-to-change-permisisons t)
   (wdired-create-parent-directories t)))

(use-package dired-subtree
  :after dired
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("<C-tab>" . dired-subtree-cycle)
        ("<S-iso-lefttab>" . dired-subtree-remove)))

(use-package dired-x
  :straight (:type built-in)
  :custom
  ((dired-clean-up-buffers-too t)
   (dired-clean-confirm-killing-deleted-buffers t)
   (dired-x-hands-off-my-keys t)
   (dired-bind-man nil)
   (dired-bind-info nil)))

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

(provide 'cogent-dired)
