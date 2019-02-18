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

;; from http://sachachua.com/blog/2007/12/clocking-time-with-emacs-org/
(defun org-dblock-write:rangereport (params)
  "Display day-by-day time reports.
   To use, make a block like

#+BEGIN: rangereport :maxlevel 2 :tstart \"<2019-02-08 Fri>\" :tend \"<2019-02-12 Tue>\" :step \"day\"
#+END:

Then press C-c C-x C-u inside
"
  (let* ((ts (plist-get params :tstart))
         (te (plist-get params :tend))
         (ts->sec (lambda (ts)
                    (time-to-seconds
                     (apply #'encode-time (org-parse-time-string ts)))))
         (sec->ts (lambda (s)
                    (format-time-string (car org-time-stamp-formats)
                                        (seconds-to-time s))))
         (start (funcall ts->sec ts))
         (end (funcall ts->sec te))
         (step (or (plist-get params :step) "day"))
         (stepfn (pcase step
                   ("day"  (lambda (d) (+ d (* 3600 24))))
                   ("week"  (lambda (d) (+ d (* 3600 24 7))))
                   ("month" (lambda (d)
                              (->> d
                                  (funcall sec->ts)
                                  (format "incmonth(newmonth(%s))")
                                  calc-eval
                                  (parse-time-string)
                                  cdddr
                                  (cons 0) (cons 0) (cons 0)
                                  (apply #'encode-time)
                                  time-to-seconds)))
                   (_ (error "Invalid step '%s'" step)))))
    (while (<= start end)
      (let ((block-end (funcall stepfn start)))
        (save-excursion
          (insert "\n\nDAY: "
                  (funcall sec->ts start)
                  (if (not (string= step "day"))
                      (s-concat
                       " to "
                       (funcall sec->ts block-end))
                    "")
                  "\n")
          (org-dblock-write:clocktable
           (-> params
               (plist-put
                :tstart
                (funcall sec->ts start))
               (plist-put
                :tend
                (funcall sec->ts block-end))
               (plist-put :step nil)))
          (setq start block-end))))))

(require 'rx)
(defun org-dblock-write:merged-rangereport (params)
  "Like `org-dblock-write:rangereport', but puts combines the entries into one table"
  (org-dblock-write:rangereport params)
  (save-excursion
    (save-restriction
      ;; like org-narrow-to-block, but that only works for blocks like
      ;; "#+begin_*", while this is like "#+begin:"
      (let* ((case-fold-search t)
             (blockp (org-between-regexps-p
                      (rx bol (0+ blank) "#+begin")
                      (rx bol (0+ blank) "#+end"))))
        (when blockp
          (narrow-to-region (car blockp) (cdr blockp))))
      ;; point-min in the block is the #+BEGIN line, so go down one
      (goto-char (point-min)) (forward-line)
      (insert "| Day | Task | Time | Subtask time |\n")
      (insert "|-----+------+------+--------------|\n")
      (save-excursion
        (delete-matching-lines
         (rx bol (| "#+CAPTION: "
                    "| Headline"
                    "|----"
                    (: (0+ blank) eol)))))
      (while (re-search-forward (rx bol "DAY: ") nil t)
        (delete-char -5) ; Delete "DAY: " before timestamp
        ;; make timestamp be the first column in a table row
        (insert "| " ) (end-of-line) (insert " ")
        ;; join-line joins the current line with the line *above*
        ;; so we do this to merge the first line of the table
        ;; with the new row that has the timestamp in column 0
        (forward-line) (join-line)
        ;; then we need to insert an empty column for the other entries
        ;; in the table for this day
        (let ((next-start (save-excursion
                            (re-search-forward (rx bol "DAY: ") nil t))))
          (while (re-search-forward (rx bol "|") next-start t)
            (insert "| "))))
      ;; when it's all done, fix up the table
      (org-table-align))))

(provide 'cogent-orgmode)
