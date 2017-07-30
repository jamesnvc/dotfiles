;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package org
  :ensure org-plus-contrib
  :config
  (setq org-replace-disputed-keys t)
  (add-hook
   'org-mode-hook
   (lambda ()
     (visual-line-mode 1)
     (set-visual-wrap-column 80)))
  ;; Fancy bullet rendering
  (use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  (use-package org-cliplink
    :config
    (with-eval-after-load "org"
      (define-key org-mode-map (kbd "C-c M-l") 'org-cliplink))))

(provide 'cogent-orgmode)
