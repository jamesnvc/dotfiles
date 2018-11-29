;;; -*- lexical-binding: t -*-

(require 'cogent-package)
(require 'cogent-keys)

(general-nmap
  "] q" #'next-error
  "[ q" #'previous-error)

(use-package flycheck
  :init
  ;; Use it for everything except ELisp mode
  (add-hook 'find-file-hook
            (lambda ()
              (when (not (equal 'emacs-lisp-mode major-mode))
                (flycheck-mode)))))

;; Turn modeline red when Flycheck has errors.
(use-package flycheck-color-mode-line
  :commands flycheck-color-mode-line-mode
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode)
  :config
  (setq flycheck-highlighting-mode 'symbols))

(use-package helm-flycheck
  :after helm
  ;; TODO: evil bindings
  :bind (("C-c ! !" . helm-flycheck)))

(provide 'cogent-flycheck)
