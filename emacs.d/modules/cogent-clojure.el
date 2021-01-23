;;; -*- lexical-binding: t -*-
(require 'cogent-package)

(use-package clojure-mode
  :commands clojure-mode
  :hook (clojure-mode-hook . enable-paredit-mode))

(use-package cider
  :commands cider-jack-in
  :hook (cider-mode-hook . eldoc-mode)
  :config

  (when (version<= "28" emacs-version)
    (el-patch-defun cider-eldoc (&optional cb)
      "Backend function for eldoc to show argument list in the echo area."
      (let ((callback (or cb #'identity)))
        (when (and (cider-connected-p)
                   ;; don't clobber an error message in the minibuffer
                   (not (member last-command '(next-error previous-error)))
                   ;; don't try to provide eldoc in EDN buffers
                   (not (cider--eldoc-edn-file-p buffer-file-name)))
          (let* ((sexp-eldoc-info (cider-eldoc-info-in-current-sexp))
                 (eldoc-info (lax-plist-get sexp-eldoc-info "eldoc-info"))
                 (pos (lax-plist-get sexp-eldoc-info "pos"))
                 (thing (lax-plist-get sexp-eldoc-info "thing")))
            (when eldoc-info
              (if (eq (cider-eldoc-thing-type eldoc-info) 'var)
                  (funcall callback (cider-eldoc-format-variable thing eldoc-info))
                (funcall callback (cider-eldoc-format-function thing pos eldoc-info)))))))))

  (evil-set-initial-state 'cider-docview-mode 'emacs)
  (evil-set-initial-state 'cider-stacktrace-mode 'emacs)
  (setq cider-prompt-for-symbol nil)
  (setq cider-font-lock-dynamically '(macro core function var))
  (with-eval-after-load "company"
    (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)))

(use-package paredit
  :commands enable-paredit-mode paredit-mode)

(use-package flycheck-clj-kondo
  :if (executable-find "clj-kondo")
  :after clojure-mode
  :hook (clojure-mode-hook . (lambda () (require 'flycheck-clj-kondo))))

;; Like vim-fireplace
(evil-define-operator cogent/evil-cider-eval (beg end)
 "Evaluate clojure expression given by <motion> via cider."
  (cider-eval-region beg end))

(evil-define-operator cogent/evil-cider-eval-replace (beg end)
  "Evaluate clojure expression given by <motion> via cider and replace
the expression with the result."
  (let* ((exp (buffer-substring-no-properties beg end))
         (res (cider-nrepl-sync-request:eval
               exp
               nil
               (if (cider-ns-form-p exp) "user" (cider-current-ns)))))
    (delete-region beg end)
    (insert (format "%s" (lax-plist-get (rest res) "value")))))

(general-nmap 'cider-mode-map
  "go" 'cogent/evil-cider-eval
  "g!" 'cogent/evil-cider-eval-replace
  "] C-d" #'cider-find-var
  "K" #'cider-doc
  "M-r" (lambda () (interactive) (cider-load-file (buffer-file-name))))

;; (defun cogent/cljfmtns ()
;;   (make-process
;;    :name "*cljfmtns*"
;;    :command (list (executable-find "cljfmtns") buffer-file-name)))

;; (defun add-cljfmt-hook ()
;;   (add-hook 'after-save-hook #'cogent/cljfmtns nil t))

;; (when (cogent/is-exec "cljfmtns")
;;   (add-hook 'clojure-mode-hook #'add-cljfmt-hook))

(provide 'cogent-clojure)
