;;; -*- lexical-binding: t -*-

(define-key evil-normal-state-map (kbd "] q") 'next-error)
(define-key evil-normal-state-map (kbd "[ q") 'previous-error)

(use-package flycheck
  :config
  ;; Use it for everything except ELisp mode
  (add-hook 'find-file-hook
            (lambda ()
              (when (not (equal 'emacs-lisp-mode major-mode))
                (flycheck-mode)))))

;; Turn modeline red when Flycheck has errors.
(use-package flycheck-color-mode-line
  :config
  (with-eval-after-load "flycheck"
    (setq flycheck-highlighting-mode 'symbols)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(with-eval-after-load "flycheck"
  (set-face-background 'flycheck-error "#660000")
  (set-face-foreground 'flycheck-error nil)
  (set-face-background 'flycheck-warning "#331800")
  (set-face-foreground 'flycheck-warning nil)
  (require 'flycheck-color-mode-line)
  (set-face-background 'flycheck-color-mode-line-error-face "#440000")
  (set-face-background 'flycheck-color-mode-line-warning-face "#553300")
  (set-face-background 'flycheck-color-mode-line-info-face nil)
  (set-face-foreground 'flycheck-color-mode-line-error-face "#ffffff")
  (set-face-foreground 'flycheck-color-mode-line-warning-face "#ffffff")
  (set-face-foreground 'flycheck-color-mode-line-info-face nil))

(with-eval-after-load "helm"
  (use-package helm-flycheck
    ;; TODO: evil bindings
    :bind (("C-c ! !" . helm-flycheck))))

(provide 'cogent-flycheck)
