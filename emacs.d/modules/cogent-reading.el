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

(autoload 'ispell-get-word "ispell")

(defun cogent/lookup-word (word)
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (save-excursion (car (ispell-get-word nil))))))
  (split-window-below)
  (eww-browse-url (format "https://en.wiktionary.org/wiki/%s" word)))

(provide 'cogent-reading)
