;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(defun cogent/org-note-file-name ()
  (expand-file-name
   (concat (read-string "Name: ") ".org")
   "~/org/notebook/"))

(use-package org
  :demand t
  :config
  (require 'org-tempo) ;; for expanding templates
  (require 'ox-beamer)
  (setq org-replace-disputed-keys t)
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (add-hook
   'org-mode-hook
   (lambda ()
     (let ((default-pred electric-pair-inhibit-predicate))
       (setq-local electric-pair-inhibit-predicate
                   (lambda (c) (if (char-equal c ?<) t default-pred))))
     (visual-line-mode 1)
     (set-visual-wrap-column 120)))

  :general
  (general-nmap :keymaps 'org-mode-map
    "<return>" #'org-return)
  (general-nvmap :prefix "SPC o"
    "a" #'org-agenda
    "c" #'org-capture))

(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-cliplink
  :after org
  :config
  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-c M-l") #'org-cliplink)))

;; add :async to BEGIN_SRC blocks & they run asynchronously!
(use-package ob-async
  :after org)

(use-package org-tree-slide
  :commands (org-tree-slide-mode org-tree-slide-skip-done-toggle))
(general-def :keymaps 'org-mode-map
  "<f8>" #'org-tree-slide-mode)

(use-package htmlize)

(defun cogent/org-inline-css-hook (exporter)
  "Make <pre> blocks in exported HTML have the same background colour
as my default face, so it will be readable"
  (when (eq exporter 'html)
    (let ((pre-bg (face-background 'default))
          (pre-fg (face-foreground 'default)))
      (setq org-html-head-include-default-style nil)
      (setq org-html-head
            (format "<style type=\"text/css\">\n pre.src { background-color: %s; color: %s}</style>\n"
                    pre-bg pre-fg)))))
(add-hook 'org-export-before-processing-hook #'cogent/org-inline-css-hook)

(use-package evil-org
  :after org
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . evil-org-set-key-theme)))

(provide 'cogent-orgmode)
