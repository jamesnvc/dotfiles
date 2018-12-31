;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(defun cogent/disable-display-line-numbers-mode ()
  (display-line-numbers-mode -1))

(use-package pdf-tools
  :demand t
  :config
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook #'cogent/disable-display-line-numbers-mode))


(provide 'cogent-reading)
