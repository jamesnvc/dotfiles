;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(electric-pair-mode 1)

(setopt show-paren-context-when-offscreen t) ; other options 'overlay, 'child-frame

;; A key for intelligently shrinking whitespace.
;; See https://github.com/jcpetkovich/shrink-whitespace.el for details.
(use-package shrink-whitespace
  :commands shrink-whitespace
  :bind ("C-c DEL" . shrink-whitespace))

;; Highlight changed areas with certain operations, such as undo, kill, yank.
(use-package volatile-highlights
  :commands volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; A function for easily editing a file as root through TRAMP.
(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(use-package emacs
  :config
  (setq-default bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t)
  (when (fboundp 'completion-preview-mode)
    (add-hook 'prog-mode-hook #'completion-preview-mode)))

(use-package so-long
  :straight (:type built-in)
  :config
  (global-so-long-mode 1))

(use-package smartscan
  :commands (smartscan-mode)
  :hook ((prog-mode-hook . smartscan-mode)))

(use-package treesit
  :straight (:type built-in)
  :config
  (setopt treesit-auto-install-grammar 'ask)
  (setopt treesit-enabled-modes t))

(comment
 (treesit-install-language-grammar 'python)
 (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
 )

(use-package combobulate
  :straight (combobulate
             :type git
             :host github
             :repo "mickeynp/combobulate"
             :branch "development")
  :config
  (add-hook 'python-ts-mode-hook #'combobulate-mode)
  (add-hook 'css-ts-mode-hook #'combobulate-mode)
  (add-hook 'c-ts-mode-hook #'combobulate-mode)
  (add-hook 'yaml-ts-mode-hook #'combobulate-mode)
  (add-hook 'js-ts-mode-hook #'combobulate-mode))

(use-package jinx
  :straight (jinx
             :type git
             :host github
             :repo "minad/jinx"
             :branch "main"
             :files (:defaults "jinx-mod.c" "emacs-module.h"))
  :bind ([remap ispell-word] . jinx-correct)
  :config
  (evil-define-motion cogent/evil-prev-jinx-error (count)
    "Go to the COUNT'th spelling error preceding point."
    :jump t
    (jinx-previous (or count 1)))

  (evil-define-motion cogent/evil-next-jinx-error (count)
    "Go to the COUNT'th spelling error succeeding point."
    :jump t
    (jinx-next (or count 1)))

  (keymap-set evil-motion-state-map "[ j" 'cogent/evil-prev-jinx-error)
  (keymap-set evil-motion-state-map "] j" 'cogent/evil-next-jinx-error))


;; (use-package cc-isearch-menu
;;   :straight (cc-isearch-menu
;;              :type git
;;              :host github
;;              :repo "kickingvegas/cc-isearch-menu"
;;              :branch "main")
;;   :config
;;   (keymap-set isearch-mode-map "<f2>" #'cc-isearch-menu-transient))

(provide 'cogent-editing)
