;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(defun cogent/org-note-file-name ()
  (expand-file-name
   (concat (read-string "Name: ") ".org")
   "~/org/notebook/"))

(straight-use-package '(org))
(straight-use-package '(org-contrib))
(progn
  ;; :demand t
  ;; :config

  (customize-set-variable 'org-catch-invisible-edits 'show-and-error)
  (customize-set-variable 'org-adapt-indentation nil)
  (customize-set-variable 'org-agenda-compact-blocks t)
  (customize-set-variable 'org-agenda-restore-windows-after-quit t)
  (customize-set-variable 'org-agenda-show-future-repeats nil)
  (customize-set-variable 'org-agenda-skip-scheduled-if-done t)
  (setopt org-edit-src-content-indentation 0)
  (customize-set-variable
   'org-agenda-custom-commands
   `(("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil)
     ("N" "Notebook search" tags ""
      ((org-agenda-files '("~/org/notebook"))))
     ("A" "Daily agenda and top priority tasks"
      ((tags-todo "*"
                  ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                   (org-agenda-skip-function
                    `(org-agenda-skip-entry-if
                      'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                   (org-agenda-block-separator nil)
                   (org-agenda-overriding-header "Important tasks without a date\n")))
       (agenda "" ((org-agenda-span 1)
                   (org-deadline-warning-days 0)
                   (org-agenda-block-separator nil)
                   (org-scheduled-past-days 0)
                   ;; We don't need the `org-agenda-date-today'
                   ;; highlight because that only has a practical
                   ;; utility in multi-day views.
                   (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                   (org-agenda-format-date "%A %-e %B %Y")
                   (org-agenda-overriding-header "\nToday's agenda\n")))
       (agenda "" ((org-agenda-start-on-weekday nil)
                   (org-agenda-start-day "+1d")
                   (org-agenda-span 3)
                   (org-deadline-warning-days 0)
                   (org-agenda-block-separator nil)
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "\nNext three days\n")))
       (agenda "" ((org-agenda-time-grid nil)
                   (org-agenda-start-on-weekday nil)
                   ;; We don't want to replicate the previous section's
                   ;; three days, so we start counting from the day after.
                   (org-agenda-start-day "+3d")
                   (org-agenda-span 14)
                   (org-agenda-show-all-dates nil)
                   (org-agenda-time-grid nil)
                   (org-deadline-warning-days 0)
                   (org-agenda-block-separator nil)
                   (org-agenda-entry-types '(:deadline))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n"))))
      ((org-agenda-compact-blocks nil)))))

  ;; make scheduled tasks more clear
  ;; via https://whhone.com/posts/org-agenda-repeated-tasks/
  (setopt org-agenda-scheduled-leaders '("Sched" "S.%2dx"))
  (setopt org-agenda-deadline-leaders '("Deadl" "In%2dd" "D.%2dx"))

  (defun cogent/org-agenda-repeater ()
    "Repeater to show in org-agenda-prefix for agenda."
    (if (org-before-first-heading-p)
        "-------"
      (format "%5s: " (or (org-get-repeat) ""))))

  (setcdr (assoc 'agenda org-agenda-prefix-format)
          " %i %-12:c%?-12t%s%(cogent/org-agenda-repeater)")

  ;; fix for babel gnuplot -- it has (:session) in it, which causes an
  ;; error because if the cdr is a list, it tries to funcall it
  ;; (setq org-babel-default-header-args:gnuplot
  ;;       '((:results . "file") (:exports . "results")))
  (setq org-babel-default-header-args:R
        '((:results . "output graphics file")))

  (add-hook
   'org-mode-hook
   (lambda ()
     (let ((default-pred electric-pair-inhibit-predicate))
       (setq-local electric-pair-inhibit-predicate
                   (lambda (c) (if (char-equal c ?<) t default-pred))))
     (visual-line-mode 1)))
  (advice-add 'org-meta-return :before #'org-end-of-line)
  (add-hook 'org-mode-hook (lambda () (setq-local line-spacing 0.1)))

  (setq org-plantuml-jar-path (expand-file-name "~/bin/plantuml.jar"))
  (with-eval-after-load 'org-src
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

  (add-hook 'org-babel-after-execute-hook (lambda ()
                                            (when org-inline-image-overlays
                                              (org-redisplay-inline-images))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)
     (ruby . t)
     (js . t)
     (plantuml . t)
     (gnuplot . t)
     (R . t)
     (python . t)
     (clojure . t)
     (emacs-lisp . t)))

  (add-to-list 'org-latex-packages-alist
               '("AUTO" "babel" t ("pdflatex")))
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "polyglossia" t ("xelatex" "lualatex")))

  (with-eval-after-load 'ob-js
    (setq org-babel-js-function-wrapper
          "process.stdout.write(require('util').inspect(function(){\n%s\n}()));" ))

  ;; Stop ctags from hijacking link behaviour and breaking internal links
  (with-eval-after-load 'org-ctags
    (setq org-open-link-functions nil))

  ;; because we want embark to take that key
  (define-key org-mode-map (kbd "C-,") nil)

  ;; :general
  (general-nmap :keymaps 'org-mode-map
    "<return>" #'org-return)
  (general-nvmap :prefix "SPC o"
    "a" #'org-agenda
    "c" #'org-capture)
  (general-define-key
   :keymaps 'org-mode-map
   "C-c C-x C-r" #'org-clock-remove-overlays
   "C-c w" #'org-refile
   "C-c <up>" #'org-shiftup
   "C-c <down>" #'org-shiftdown))

(use-package org-modern
  :straight (org-modern
             :type git
             :host github
             :repo "minad/org-modern"
             :branch "main")
  :after org
  :commands org-modern-mode
  :config
  (setopt org-modern-hide-stars nil)
  :init
  (add-hook 'org-mode-hook #'org-modern-mode))

(use-package org-cliplink
  :after org
  :config
  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-c M-l") #'org-cliplink))

  (setopt org-modern-table nil))

;; add :async to BEGIN_SRC blocks & they run asynchronously!
(use-package ob-async
  :after org
  :straight (ob-async
             :type git
             :host github
             :repo "jamesnvc/ob-async"
             :branch "async-variables-fix"))

(use-package ob-swiftui
  :after org
  :straight (ob-swiftui
             :type git
             :host github
             :repo "xenodium/ob-swiftui"
             :branch "main"))


(use-package htmlize)

(use-package gnuplot
  :after org-plot)

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
  :init
  ;; ;; work around removal of evil-redirect-digit-argument from evil
  ;; (fset 'evil-redirect-digit-argument 'ignore)
  ;; (add-to-list 'evil-digit-bound-motions 'evil-org-beginning-of-line)
  (evil-define-key 'motion 'evil-org-mode
    (kbd "0") 'evil-org-beginning-of-line)

  :hook ((org-mode-hook . evil-org-mode)
         (evil-org-mode-hook . evil-org-set-key-theme)))

;; from http://sachachua.com/blog/2007/12/clocking-time-with-emacs-org/
(defun org-dblock-write:rangereport (params)
  "Display day-by-day time reports.
   To use, make a block like

#+BEGIN: rangereport :maxlevel 2 :tstart \"<2019-02-08 Fri>\" :tend \"<2019-02-12 Tue>\" :step \"day\"
#+END:

Then press C-c C-x C-u inside
"
  (let* ((ts->sec (lambda (ts)
                    (time-to-seconds
                     (apply #'encode-time (org-parse-time-string ts)))))
         (sec->ts (lambda (s)
                    (format-time-string (car org-time-stamp-formats)
                                        (seconds-to-time s))))
         (ts (plist-get params :tstart))
         (te (or (plist-get params :tend)
                 (funcall sec->ts (current-time))))
         (start (funcall ts->sec ts))
         (end (funcall ts->sec te))
         (step (or (plist-get params :step) "day"))
         (step-forward (pcase step
                   ("day"  (lambda (d) (+ d (* 3600 24))))
                   ("week"  (lambda (d) (+ d (* 3600 24 7))))
                   ("month" (lambda (d)
                              (thread-last d
                                (funcall sec->ts)
                                (format "incmonth(newmonth(%s))")
                                calc-eval
                                (parse-time-string)
                                cdddr
                                (cons 0) (cons 0) (cons 0)
                                (apply #'encode-time)
                                time-to-seconds)))
                   (_ (error "Invalid step '%s'" step))))
         (step-backward (pcase step
                      ("day"  (lambda (d) (- d (* 3600 24))))
                      ("week"  (lambda (d) (- d (* 3600 24 7))))
                      ("month" (lambda (d)
                                 (thread-last d
                                              (funcall sec->ts)
                                              (format "decmonth(newmonth(%s))")
                                              calc-eval
                                              (parse-time-string)
                                              cdddr
                                              (cons 0) (cons 0) (cons 0)
                                              (apply #'encode-time)
                                              time-to-seconds)))
                      (_ (error "Invalid step '%s'" step))))
         (block-start (funcall step-backward end)))
    (while (<= start block-start)
      (let ((block-end (funcall step-forward block-start)))
        (save-excursion
          (insert "\n\nDAY: "
                  (funcall sec->ts block-start)
                  (if (not (string= step "day"))
                      (concat " to " (funcall sec->ts block-end))
                    "")
                  "\n")
          (org-dblock-write:clocktable
           (thread-first params
             (plist-put :tstart (funcall sec->ts block-start))
             (plist-put :tend (funcall sec->ts block-end))
             (plist-put :step nil)))
          (setq block-start (funcall step-backward block-start)))))))

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
      (delete-matching-lines "0:00" (point-min) (point-max))
      ;; when it's all done, fix up the table
      (org-table-align))))

(use-package org-drill
  :straight (org-drill
             :type git
             :host gitlab
             :repo "phillord/org-drill")
  :defer t
  :commands (org-drill))

(use-package org-tree-slide
  :defer t
  :commands (org-tree-slide-mode)
  :custom (org-tree-slide-slide-in-effect nil))

(use-package olivetti
  :defer t
  :commands (olivetti-mode)
  :custom ((olivetti-body-width 0.5)
           (olivetti-minimum-body-width 80)))

(defun cogent/collapse-plantuml-blocks ()
  (interactive)
  (org-block-map
   (lambda ()
     (when (org-in-src-block-p)
       (let ((line (buffer-substring-no-properties
                    (point)
                    (save-excursion (end-of-line) (point)))))
         (when (string-prefix-p "#+begin_src plantuml" line)
           (org-hide-block-toggle t)))))))

(defun cogent/start-presentation ()
  "Start a presentation with org-tree-slide"
  (interactive)
  (setq mode-line-format nil)
  (display-line-numbers-mode -1)
  (dolist (face '(default fixed-pitch variable-pitch))
    (set-face-attribute face (selected-frame) :height 200))
  (when (derived-mode-p 'text-mode)
    (variable-pitch-mode +1))
  (olivetti-mode +1)
  (org-redisplay-inline-images)
  (cogent/collapse-plantuml-blocks)
  (when (eq 'org-mode major-mode)
    (org-tree-slide-mode 1)
    (org-indent-mode 1)))

(defun cogent/stop-presentation ()
  "Start a presentation with org-tree-slide"
  (interactive)
  (variable-pitch-mode -1)
  (setq mode-line-format (default-value 'mode-line-format))
  (display-line-numbers-mode 1)
  (olivetti-mode -1)
  (dolist (face '(default fixed-pitch variable-pitch))
    (set-face-attribute face (selected-frame) :height 80))
  (when (eq 'org-mode major-mode)
    (org-tree-slide-mode -1)
    (org-indent-mode -1)))

(defun cogent/org-element-logbook-duration ()
  "Get the logged duration for the element at point"
  (interactive)
  (save-excursion
    (unless (org-at-heading-p)
      (outline-previous-heading))
    (when (re-search-forward ":LOGBOOK:" (save-excursion
                                           (outline-next-heading)
                                           (point))
                             t)
      (let* ((drawer (org-element-property-drawer-parser nil))
             (beg (org-element-property :contents-begin drawer))
             (end (org-element-property :contents-end drawer))
             (duration-minutes 0))
        (save-excursion
          (goto-char beg)
          (while (< (point) end)
            (let ((clock (org-element-at-point)))
              (when-let ((duration (org-element-property :duration clock)))
                (incf duration-minutes
                      (org-duration-string-to-minutes duration))))
            (forward-line 1)))
        (org-duration-from-minutes duration-minutes)))))

(use-package org-ql)

(use-package org-clock-export
  :straight (org-clock-export
             :type git
             :host github
             :repo "jamesnvc/org-clock-export"
             :branch "escape-fields"))

(use-package ox-epub
  :after org)

(provide 'cogent-orgmode)
