;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package pdf-tools
  :defer t
  :straight (pdf-tools
             :type git
             :host github
             :repo "vedang/pdf-tools")
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

(comment
 (use-package nov
  :defer t
  :commands nov-mode
  :config
  (evil-set-initial-state 'nov-mode 'emacs)
  :mode ("\\.epub\\'" . nov-mode)))

(use-package reader
  :straight '(reader :type git
                     :host codeberg
                     :repo "divyaranjan/emacs-reader"
                     :files ("*.el" "render-core.dylib")
                     :pre-build ("make" "all"))
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . reader-mode)))

(defvar infu-bionic-reading-face nil "a face for `infu-bionic-reading-region'.")

(setq infu-bionic-reading-face 'bold)

(defun infu-bionic-reading-buffer ()
  "Bold the first few chars of every word in current buffer.
Version 2022-05-21"
  (interactive)
  (infu-bionic-reading-region (point-min) (point-max)))

(defun infu-bionic-reading-region (begin end)
  "Bold the first few chars of every word in region.
Version 2022-05-21"
  (interactive "r")
  (save-restriction
    (narrow-to-region begin end)
    (goto-char (point-min))
    (while (forward-word)
      (pcase-let ((`(,word-begin . ,word-end) (bounds-of-thing-at-point 'word)))
        ;; bold the first half of the word to the left of cursor
        (put-text-property
         word-begin
         (+ word-begin (1+ (/ (- word-end word-begin) 2)))
         'font-lock-face infu-bionic-reading-face)))))

(provide 'cogent-reading)
