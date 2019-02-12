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

(defun org-dblock-write:rangereport (params)
  "Display day-by-day time reports.
   To use, make a block like

#+BEGIN: rangereport :maxlevel 2 :tstart \"<2019-02-08 Fri>\" :tend \"<2019-02-12 Tue>\"
#+END:

Then press C-c C-x C-u inside
"
  (let* ((ts (plist-get params :tstart))
         (te (plist-get params :tend))
         (start (time-to-seconds
                 (apply 'encode-time (org-parse-time-string ts))))
         (end (time-to-seconds
               (apply 'encode-time (org-parse-time-string te))))
         day-numbers)
    (setq params (plist-put params :tstart nil))
    (setq params (plist-put params :end nil))
    (while (<= start end)
      (save-excursion
        (insert "\n\nDAY: "
                (format-time-string (car org-time-stamp-formats)
                                    (seconds-to-time start))
                "\n")
        (org-dblock-write:clocktable
         (plist-put
          (plist-put
           params
           :tstart
           (format-time-string (car org-time-stamp-formats)
                               (seconds-to-time start)))
          :tend
          (format-time-string (car org-time-stamp-formats)
                              (seconds-to-time end))))
        (setq start (+ 86400 start))))))

(require 'rx)
(defun org-dblock-write:merged-rangereport (params)
  (org-dblock-write:rangereport params)
  (message "WROTE RANGEREPORT")
  (save-excursion
    (save-restriction
      (let* ((case-fold-search t)
             (blockp (org-between-regexps-p
                      (rx bol (0+ blank) "#+begin")
                      (rx bol (0+ blank) "#+end"))))
        (when blockp
          (narrow-to-region (car blockp) (cdr blockp))))
      (goto-char (point-min))
      (forward-line)
      (insert "| Day | Task | Time | Subtask time |\n")
      (insert "|-----+------+------+--------------|\n")
      (save-excursion
        (delete-matching-lines (rx bol "#+CAPTION: ")))
      (save-excursion
        (delete-matching-lines (rx bol "| Headline")))
      (save-excursion
        (delete-matching-lines (rx bol "|----")))
      (save-excursion
        (delete-matching-lines (rx bol (0+ blank) eol)))
      (while (re-search-forward (rx bol "DAY: ") nil t)
        (message "DAY")
        (delete-char -5)
        (insert "| " )
        (end-of-line)
        (insert " ")
        (forward-line)
        (join-line)
        (let ((next-start (save-excursion
                            (re-search-forward (rx bol "DAY: ") nil t))))
          (message "NEXT START " next-start)
          (while (re-search-forward (rx bol "|") next-start t)
            (insert "| "))))
      (org-table-align))))

(provide 'cogent-orgmode)
