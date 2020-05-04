;;; -*- lexical-binding: t -*-

(require 'cogent-package)
(require 'cogent-keys)

(general-nmap
  "] q" #'next-error
  "[ q" #'previous-error)

(use-package flycheck
  :hook
  ;; Use it for everything except ELisp mode
  (find-file . (lambda ()
                 (when (not (equal 'emacs-lisp-mode major-mode))
                   (flycheck-mode))))

  :config
  (evil-define-minor-mode-key 'normal 'flycheck-mode
   (kbd "[ q") #'flycheck-previous-error
   (kbd "] q") #'flycheck-next-error))

;; Turn modeline red when Flycheck has errors.
(use-package flycheck-color-mode-line
  :commands flycheck-color-mode-line-mode
  :hook (flycheck-mode . flycheck-color-mode-line-mode)
  :config
  (setq flycheck-highlighting-mode 'symbols))

(use-package helm-flycheck
  :after helm
  ;; TODO: evil bindings
  :bind (("C-c ! !" . helm-flycheck)))

(provide 'cogent-flycheck)
