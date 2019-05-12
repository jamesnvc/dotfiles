;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(defun cogent/disable-display-line-numbers-mode ()
  (display-line-numbers-mode -1))

(use-package pdf-tools
  :commands (pdf-view-mode pdf-tools-install)
  :init
  (add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdf-view-mode))
  (add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))
  :config
  (pdf-tools-install)
  :hook (pdf-view-mode . cogent/disable-display-line-numbers-mode))


(provide 'cogent-reading)
