;;; -*- lexical-binding: t -*-

(require 'cogent-package)
(require 'cogent-keys)

(use-package paredit
  :commands paredit-mode enable-paredit-mode
  :hook (emacs-lisp-mode . enable-paredit-mode)
  :diminish paredit-mode)

(use-package highlight-parentheses
  :commands highlight-parentheses-mode
  :hook (emacs-lisp-mode . highlight-parentheses-mode)
  :diminish highlight-parentheses-mode)

(defun cogent-elisp/remove-elc-on-save ()
  "If you're saving an elisp file, you probably want to remove the
now-invalid elc file"
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (when (file-exists-p (concat buffer-file-name "c"))
                (delete-file (concat buffer-file-name "c"))))))
(add-hook 'emacs-lisp-mode-hook #'cogent-elisp/remove-elc-on-save)

;; Show context-based docs in minibuffer
(add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)

;; Show the result of evaluating as an overlay in the elisp buffer
(use-package eros
  :commands eros-mode
  :hook (emacs-lisp-mode . eros-mode))

(evil-define-operator cogent/evil-elisp-eval (beg end)
  (eros--eval-overlay
   (eval-expression (read (buffer-substring-no-properties beg end))
                    nil
                    nil)
   end))

(evil-define-operator cogent/evil-elisp-eval-replace (beg end)
  (let ((exp (read (buffer-substring-no-properties beg end))))
    (delete-region beg end)
    (eros--eval-overlay
     (eval-expression exp t nil)
     (point))))

(general-nmap '(emacs-lisp-mode-map lisp-interaction-mode-map)
  ;; Like vim-unimpaired
  "] C-d" 'find-function-at-point
  "go" 'cogent/evil-elisp-eval
  "g!" 'cogent/evil-elisp-eval-replace
  "c" (general-key-dispatch 'evil-change
        "c" #'evil-change-whole-line
        "r-" #'cogent/kebab-case
        "r_" #'cogent/snake-case
        "rc" #'cogent/camel-case
        "rC" #'cogent/camel-case-upper))
(general-vmap 'emacs-lisp-mode-map "c" 'evil-change)


(provide 'cogent-elisp)
