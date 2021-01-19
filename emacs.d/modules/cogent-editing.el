;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(electric-pair-mode 1)

;; A key for intelligently shrinking whitespace.
;; See https://github.com/jcpetkovich/shrink-whitespace.el for details.
(use-package shrink-whitespace
  :commands shrink-whitespace
  :bind ("C-c DEL" . shrink-whitespace))

;; Highlight changed areas with certain operations, such as undo, kill, yank.
(use-package volatile-highlights
  :commands volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; A function for easily editing a file as root through TRAMP.
(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (if (fboundp 'helm-read-file-name)
                             (helm-read-file-name "File: ")
                           (ido-read-file-name "File: "))))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(use-package emacs
  :config
  (setq-default bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t))

(use-package so-long
  :straight (:type built-in)
  :config
  (global-so-long-mode 1))

(provide 'cogent-editing)
