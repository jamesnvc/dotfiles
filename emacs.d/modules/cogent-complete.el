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

  ;; Make company look good in dark mode
  (set-face-foreground 'company-tooltip "#000")
  (set-face-background 'company-tooltip "#ddd")
  (set-face-background 'company-scrollbar-bg "#fff")
  (set-face-background 'company-scrollbar-fg "#999")
  (set-face-background 'company-tooltip-selection "#aaa")
  (set-face-foreground 'company-tooltip-common "#9a0000")
  (set-face-foreground 'company-tooltip-common-selection "#9a0000")
  (set-face-foreground 'company-tooltip-annotation "#00008e")

  :general
  (:keymaps 'company-active-map
            "C-n" #'company-select-next
            "C-p" #'company-select-previous
            "C-w" #'evil-delete-backward-word)
  (:keymaps 'company-active-map :states 'insert
            "C-w" #'evil-delete-backward-word))

(use-package company-quickhelp
  :after company
  :config
  (setq company-quickhelp-delay 1)
  (company-quickhelp-mode 1))

(use-package company-emoji
  :after company
  :config
  (company-emoji-init)
  (require 'cl-seq)
  (setf company-emojis (cl-remove-if (lambda (elt) (string-equal ":-1:" elt))  company-emojis)))

;; invoke to gather completion candidates from multiple sources if
;; active source isn't giving us anything
(use-package company-try-hard
  :after company
  :commands company-try-hard
  :general
  ("C-\\" #'company-try-hard)
  (:keymaps 'company-active-map
            "C-\\" #'company-try-hard
            "C-n" #'company-select-next
            "C-p" #'company-select-previous)
  :diminish company-mode)

(provide 'cogent-complete)
