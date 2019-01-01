;;; -*- lexical-binding: t -*-
(require 'cogent-package)

;; theme switching stuff from https://www.greghendershott.com/2017/02/emacs-themes.html
(defun cogent/disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defvar cogent/theme-hooks nil
  "((theme-id . function) ...)")

(defun cogent/add-theme-hook (theme-id hook-func)
  (add-to-list 'cogent/theme-hooks (cons theme-id hook-func)))

(defun cogent/load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhance `load-theme' by disabling other enabled themes & calling hooks"
  (unless no-enable
    (cogent/disable-all-themes))
  (prog1
      (apply f theme-id no-confirm no-enable args)
    (unless no-enable
      (pcase (assq theme-id cogent/theme-hooks)
        (`(,_ . ,f) (funcall f))))))

(advice-add 'load-theme :around #'cogent/load-theme-advice)

(use-package dracula-theme
  :defer t
  :init
  (defun cogent/dracula-theme-hook ()
    (set-face-background 'powerline-active1 "gray9")
    (set-face-background 'powerline-active2 "gray13")
    (set-face-attribute 'line-number nil :foreground "#678")
    (set-face-attribute 'line-number-current-line nil :foreground "#96989c")
    (set-face-background 'powerline-inactive2 "#282a36")
    ;; Comments exist for a reason - make them stand out
    (set-face-attribute 'font-lock-comment-face nil :weight 'semi-bold)
    (with-eval-after-load 'org
      (set-face-background 'org-block-begin-line "#44475a")))
  (cogent/add-theme-hook 'dracula #'cogent/dracula-theme-hook))

(use-package solarized
  :straight solarized-theme
  :defer t)

(with-eval-after-load 'term
  (set-face-foreground 'term-color-black "#3f3f3f")
  (set-face-foreground 'term-color-red "#cc9393")
  (set-face-foreground 'term-color-green "#7f9f7f")
  (set-face-foreground 'term-color-yellow "#f0dfaf")
  (set-face-foreground 'term-color-blue "#8cd0d3")
  (set-face-foreground 'term-color-magenta "#dc8cc3")
  (set-face-foreground 'term-color-cyan "#93e0e3")
  (set-face-foreground 'term-color-white "#dcdccc"))

(setq redisplay-dont-pause t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(global-display-line-numbers-mode)

(setq column-number-mode t)

(setq linum-disabled-modes
      '(term-mode slime-repl-mode magit-status-mode help-mode nrepl-mode
        mu4e-main-mode mu4e-headers-mode mu4e-view-mode
        mu4e-compose-mode))

(show-paren-mode 1)

(use-package nyan-mode
  :config
  (nyan-mode 1)
  (setq nyan-bar-length 16
        nyan-wavy-trail nil))

(use-package diminish)

(with-eval-after-load 'eldoc (diminish 'eldoc-mode))
(with-eval-after-load 'autopair (diminish 'autopair-mode))
(with-eval-after-load 'abbrev (diminish 'abbrev-mode))
(with-eval-after-load 'js2-highlight-vars (diminish 'js2-highlight-vars-mode))
(with-eval-after-load 'mmm-mode (diminish 'mmm-mode))
(with-eval-after-load 'skewer-html (diminish 'skewer-html-mode))
(with-eval-after-load 'skewer-mode (diminish 'skewer-mode))
(with-eval-after-load 'auto-indent-mode (diminish 'auto-indent-minor-mode))
;; (eval-after-load "subword" '(diminish 'subword-mode))
(with-eval-after-load 'cider (diminish 'cider-mode "🤖"))
(with-eval-after-load 'smartparens (diminish 'smartparens-mode))
(with-eval-after-load 'undo-tree (diminish 'undo-tree-mode ""))
(with-eval-after-load 'flycheck (diminish 'flycheck-mode))
(with-eval-after-load 'git-gutter+ (diminish 'git-gutter+-mode ""))
(with-eval-after-load 'evil-mc (diminish 'evil-mc-mode))
(with-eval-after-load 'geiser (diminish 'geiser-autodoc-mode))
(with-eval-after-load 'company (diminish 'company-mode))
(with-eval-after-load 'flyspell (diminish 'flyspell-mode ""))
(with-eval-after-load 'alchemist (diminish 'alchemist-mode ""))
(diminish 'visual-line-mode "⮓")
(diminish 'auto-revert-mode)

(use-package cyphejor
  :config
  (setq cyphejor-rules
        '(("mode" "")
          ("haskell" "")
          ("emacs" "")
          ("sh" "")
          ("ruby" "")
          ("magit" "")
          ("clojure" "")
          ("markdown" "")
          ("js2" "")
          ("sql" "")
          ("dired" "")
          ("eshell" "")
          ("html" "")
          ("rust" "")
          ("swift" "")
          ("erlang" "")
          ("elixir" "")
          ("alchemist" "")
          ("erc" "")
          ("notmuch" "")))
  (cyphejor-mode 1))

;; Handle ANSI colours in compile buffer output.
;; From https://gist.github.com/jwiegley/8ae7145ba5ce64250a05
(defun compilation-ansi-color-process-output ()
  (ansi-color-process-output nil)
  (set (make-local-variable 'comint-last-output-start)
       (point-marker)))
(add-hook 'compilation-filter-hook #'compilation-ansi-color-process-output)

(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme)
  (spaceline-helm-mode 1)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-major-mode-on)
  (spaceline-toggle-version-control-on)
  (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state
        spaceline-window-numbers-unicode t
        spaceline-workspace-numbers-unicode t))

(with-eval-after-load 'dash (dash-enable-font-lock))

(cond
 ((member "FSD Emoji" (font-family-list))
  (set-fontset-font t 'unicode "FSD Emoji" nil 'prepend))
 ((member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend)))

(with-eval-after-load "flycheck"
  (set-face-background 'flycheck-error "#660000")
  (set-face-foreground 'flycheck-error nil)
  (set-face-background 'flycheck-warning "#331800")
  (set-face-foreground 'flycheck-warning nil))

(with-eval-after-load "flycheck-color-mode-line"
  (set-face-background 'flycheck-color-mode-line-error-face "#440000")
  (set-face-background 'flycheck-color-mode-line-warning-face "#553300")
  (set-face-background 'flycheck-color-mode-line-info-face nil)
  (set-face-foreground 'flycheck-color-mode-line-error-face "#ffffff")
  (set-face-foreground 'flycheck-color-mode-line-warning-face "#ffffff")
  (set-face-foreground 'flycheck-color-mode-line-info-face nil))

(load-theme 'dracula t)

(provide 'cogent-appearance)
