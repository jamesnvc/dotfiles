;;; -*- lexical-binding: t -*-

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(require 'iso-transl)

(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat dotfiles-dir "bak"))))

      undo-tree-history-directory-alist
      `(("." . ,(expand-file-name (concat dotfiles-dir "undo")))))

(setq compilation-ask-about-save nil)

(delete-selection-mode t)
(transient-mark-mode t)

(global-auto-revert-mode 1)

(setq-default
 browse-url-browser-function (quote browse-url-generic)
 browse-url-generic-program "xdg-open")

(provide 'cogent-general)
