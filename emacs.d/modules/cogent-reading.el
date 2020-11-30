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
  :hook ((pdf-view-mode-hook . (lambda () (display-line-numbers-mode -1)))
         (pdf-view-mode-hook . pdf-tools-enable-minor-modes)))

(use-package org-pdftools
  :defer t
  :hook (org-load-hook . org-pdftools-setup-link))

(use-package nov
  :defer t
  :commands nov-mode
  :config
  (evil-set-initial-state 'nov-mode 'emacs)
  :mode ("\\.epub\\'" . nov-mode))

(provide 'cogent-reading)
