;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package pdf-tools
  :demand t
  :config
  (pdf-tools-install))

(defun cogent/display-display-line-numbers-mode ()
  (display-line-numbers-mode -1))

(add-hook 'pdf-view-mode-hook #'display-line-numbers-mode)

(provide 'cogent-reading)
