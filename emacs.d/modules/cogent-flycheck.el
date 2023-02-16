;;; -*- lexical-binding: t -*-

(require 'cogent-package)
(require 'cogent-keys)

(defun cogent/remove-error-highlight-overlay ()
  (interactive)
  (when next-error--message-highlight-overlay
    (delete-overlay next-error--message-highlight-overlay)))

(general-nmap
  "] q" #'next-error
  "[ q" #'previous-error
  "SPC [" #'cogent/remove-error-highlight-overlay)

(use-package flycheck
  :config
  (evil-define-minor-mode-key 'normal 'flycheck-mode
   (kbd "[ q") #'flycheck-previous-error
   (kbd "] q") #'flycheck-next-error
   (kbd "[ Q") #'flycheck-first-error)
  (add-hook 'sh-mode-hook #'flycheck-mode))

;; Turn modeline red when Flycheck has errors.
(use-package flycheck-color-mode-line
  :commands flycheck-color-mode-line-mode
  :hook (flycheck-mode-hook . flycheck-color-mode-line-mode)
  :config
  (setq flycheck-highlighting-mode 'symbols))

(provide 'cogent-flycheck)
