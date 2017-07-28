;;; -*- lexical-binding: t -*-
(require 'cogent-package)

(use-package clojure-mode
  :commands clojure-mode
  :config
  (add-hook
   'clojure-mode-hook
   (lambda ()
     (paredit-mode)
     (clj-refactor-mode 1)
     ; start an nREPL elsewhere, use M-x monroe to connect
     (use-package monroe
       :commands monroe
       :config
       (clojure-enable-monroe)
       :bind (:map clojure-mode-map ("C-x C-e" . monroe-eval-expression-at-point)))
     (use-package clj-refactor
       :commands clj-refactor-mode
       ;; TODO: reasonable bindings for evil?
       )))
  ;; Add completion using monroe + company-mode
  (with-eval-after-load "company"
    (defun monroe-eval-string (s callback)
      (monroe-send-eval-string
       s
       (lambda (response)
	 (monroe-dbind-response
	  response (id ns value err out ex root-ex status)
	  (when ns (setq monroe-buffer-ns ns))
	  (when value (funcall callback nil value))
	  (when status
	    (when (member "eval-error" status) (funcall callback ex nil))
	    (when (member "interrupted" status) (funcall callback status nil))
	    (when (member "need-input" status) (monroe-handle-input))
	    (when (member "done" status) (remhash id monroe-requests)))))))

    (defun monroe-get-completions (word callback)
      (interactive)
      (monroe-eval-string
       (format "(complete.core/completions \"%s\")" word)
       (lambda (err s)
	 (when (not err) (funcall callback (read-from-whole-string s))))))

    (defun company-monroe (command &optional arg &rest ignored)
      (interactive (list 'interactive))
      (cl-case command
	(interactive (company-begin-backend 'company-monroe))
	(prefix (and (eq major-mode 'clojure-mode)
		     (get-buffer "*monroe-connection*")))
	(candidates
	 (cons :async
	       (lambda (callback)
		 (monroe-get-completions arg callback))))))
    (add-to-list 'company-backends 'company-monroe)))

(use-package paredit
  :commands enable-paredit-mode paredit-mode
  :diminish paredit-mode
  :init
  (add-hook 'clojure-mode-hook 'enable-paredit-mode))

(provide 'cogent-clojure)
