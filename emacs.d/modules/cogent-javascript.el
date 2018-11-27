;;; -*- lexical-binding: t -*-

(require 'cogent-package)
(require 'cogent-base)
(require 'cogent-json)

;; If npm is installed, add its local prefix to search path, to help find linters
(-when-let (npm-prefix (cogent/exec-if-exec "npm" "config get prefix"))
  (setenv "PATH" (concat npm-prefix "/bin:" (getenv "PATH")))
  (add-to-list 'exec-path (concat npm-prefix "/bin")))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
         ("\\.es6\\'" . js2-mode)
         ("\\.ejs\\'" . js2-mode))
  :interpreter "node"
  :commands js2-mode
  :config

  (setq-default
   js2-mode-indent-ignore-first-tab t
   js2-strict-inconsistent-return-warning nil
   js2-global-externs '("module" "require" "__dirname" "process" "console"
                        "JSON" "$" "_")))

(use-package rjsx-mode)

(use-package js2-refactor
  :after js2-mode
  :commands (js2r-add-keybindings-with-prefix)
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package tern
  :commands tern-mode
  :config
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  (setq tern-command (list (or (cogent/resolve-exec "tern") "tern"))))

(use-package company-tern
  :after tern company
  :config
  (add-to-list 'company-backends #'company-tern))

(provide 'cogent-javascript)
