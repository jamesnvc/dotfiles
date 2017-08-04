;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package org
  :ensure org-plus-contrib
  :config
  (setq org-replace-disputed-keys t)
  (setq org-default-notes-file (concat org-directory "/notes.org"))
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

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme))))

(provide 'cogent-orgmode)
