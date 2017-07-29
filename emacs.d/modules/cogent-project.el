;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package projectile
  :demand t
  :commands projectile-global-mode
  :config
  (projectile-global-mode)
  :bind ("C-c C-f" . projectile-find-file)
  :diminish projectile-mode)

(use-package ibuffer-projectile
  :bind ("C-x C-b" . ibuffer)
  :config
  (add-hook 'ibuffer-hook
	    (lambda ()
	      (ibuffer-projectile-set-filter-groups)
	      (unless (eq ibuffer-sorting-mode 'alphabetic)
		(ibuffer-do-sort-by-alphabetic)))))

(provide 'cogent-project)