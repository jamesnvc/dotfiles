;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package paredit
  :commands paredit-mode
  :config (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  :diminish paredit-mode)

; TODO: add evil bindings for paredit 

(use-package highlight-parentheses
  :commands highlight-parentheses-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
  :diminish highlight-parentheses-mode)

(defun cogent-elisp/remove-elc-on-save ()
  "If you're saving an elisp file, you probably want to remove the now-invalid elc file"
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
	    (lambda ()
	      (if (file-exists-p (concat buffer-file-name "c"))
		  (delete-file (concat buffer-file-name "c"))))))
(add-hook 'emacs-lisp-mode-hook 'cogent-elisp/remove-elc-on-save)

;; Show context-based docs in minibuffer
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; Show the result of evaluating as an overlay in the elisp buffer
(use-package eros
  :commands eros-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'eros-mode))

(evil-define-key 'normal emacs-lisp-mode-map (kbd "] C-d") 'find-function-at-point)

(provide 'cogent-elisp)
