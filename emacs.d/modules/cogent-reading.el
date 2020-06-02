;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package pdf-tools
  :defer t
  :commands (pdf-view-mode pdf-tools-install)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (define-pdf-cache-function pagelabels)
  :hook ((pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
         (pdf-view-mode . pdf-tools-enable-minor-modes)))

(use-package org-pdftools
  :hook (org-load . org-pdftools-setup-link))

(provide 'cogent-reading)
