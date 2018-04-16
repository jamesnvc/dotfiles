;;; -*- lexical-binding: t -*-
(require 'cogent-package)

(require 'term)

(defun cogent-appearance/dark ()
  (interactive)
  ;; (use-package material-theme)
  ;; (load-theme 'material)
  (use-package dracula-theme)

  ;; (set-face-background 'default "#000")

  ;; (set-face-background 'region "#223355")
  ;; (set-face-background 'fringe "#000")
  (set-face-attribute
   'linum nil
   :foreground "#678"
   ;; :background "#000"
   :height 0.9)
  (set-face-attribute
   'linum-highlight-face nil
   :foreground "#96989c"
   ;; :background "#263238"
   :height 0.9)
  ;(set-face-foreground 'which-func "#7f9f7f")

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

(global-linum-mode t)
(setq linum-format "%4d")
(use-package linum-relative)
(linum-relative-mode)


(use-package hlinum
  :config (hlinum-activate))

(setq column-number-mode t)

(setq linum-disabled-modes
      '(term-mode slime-repl-mode magit-status-mode help-mode nrepl-mode
        mu4e-main-mode mu4e-headers-mode mu4e-view-mode
        mu4e-compose-mode))
(defun linum-on ()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes))
    (linum-mode 1)))

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

(cogent-appearance/dark)

(cond
 ((member "FSD Emoji" (font-family-list))
  (set-fontset-font t 'unicode "FSD Emoji" nil 'prepend))
 ((member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend)))

(provide 'cogent-appearance)
