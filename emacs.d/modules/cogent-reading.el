;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package pdf-tools
  :commands (pdf-view-mode pdf-tools-install)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  :hook ((pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
         (pdf-view-mode . pdf-tools-enable-minor-modes)))


(provide 'cogent-reading)
