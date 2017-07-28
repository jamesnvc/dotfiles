;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package company
  :demand t
  :commands company-mode
  :config
  (global-company-mode)
  (setq company-global-modes '(not term-mode))
  (setq company-minimum-prefix-length 2
	company-selection-wrap-around t
	company-show-numbers t
	company-tooltip-align-annotations t
	company-require-match nil
	company-deabbrev-downcase nil
	company-dabbrev-ignore-case nil)
  (setq company-transformers '(company-sort-by-occurrence))
  (use-package company-quickhelp
    :config
    (setq company-quickhelp-delay 1)
    (company-quickhelp-mode 1))
  (use-package company-emoji
    :config
    (company-emoji-init))
  ;; Make company look good in dark mode
  (set-face-foreground 'company-tooltip "#000")
  (set-face-background 'company-tooltip "#ddd")
  (set-face-background 'company-scrollbar-bg "#fff")
  (set-face-background 'company-scrollbar-fg "#999")
  (set-face-background 'company-tooltip-selection "#aaa")
  (set-face-foreground 'company-tooltip-common "#9a0000")
  (set-face-foreground 'company-tooltip-common-selection "#9a0000")
  (set-face-foreground 'company-tooltip-annotation "#00008e") 

  ;; invoke to gather completion candidates from multiple sources if
  ;; active source isn't giving us anything
  (use-package company-try-hard
    :commands company-try-hard
    :bind ("C-\\" . company-try-hard)
    :config
    (bind-keys :map company-active-map
	       ("C-\\" . company-try-hard))
    :diminish company-mode))

(provide 'cogent-complete)
