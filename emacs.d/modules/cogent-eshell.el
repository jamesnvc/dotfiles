;;; -*- lexical-binding: t -*-

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

;; Make T function as an eshell glob to find org files with a matching tag
;; e.g. `ls *.org(T'git')`'
(defun eshell-org-file-tags ()
  "Helps the eshell parse the text the point is currently on,
looking for parameters surrounded in single quotes. Returns a
function that takes a FILE and returns nil if the file given to
it doesn't contain the org-mode #+TAGS: entry specified."
  (if (looking-at "'\\([^)']+\\)'")
      (let* ((tag (match-string 1))
             (reg (concat "^#\\+TAGS: .*" tag "\\b")))
        (goto-char (match-end 0))
        `(lambda (file)
           (with-temp-buffer
             (insert-file-contents file)
             (re-search-forward ,reg nil t 1))))
    (error "The `T' predicate takes an org-mode tag value in single quotes.")))
(add-hook 'eshell-pred-load-hook (lambda ()
  (add-to-list 'eshell-predicate-alist '(?T . (eshell-org-file-tags)))))

(provide 'cogent-eshell)
