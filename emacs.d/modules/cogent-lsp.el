;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package eglot
  :straight (:type built-in)
  :config
  (setopt eglot-server-programs (cons
                                 (cons 'prolog-mode
                                       (list "swipl"
                                             "-O"
                                             ;; "-g" "use_module(library(lsp_server))."
                                             "-s" (expand-file-name "~/Projects/prolog-lsp/prolog/lsp_server.pl")
                                             "-g" "lsp_server:main"
                                             "-t" "halt"
                                             "--" "stdio"))
                                 eglot-server-programs)))

(use-package flymake
  :straight (:type built-in)
  :config
  (evil-define-motion cogent/evil-prev-flymake-error (count)
    "Go to the COUNT'th flymake error preceding point."
    :jump t
    (flymake-goto-prev-error (or count 1) nil t))

  (evil-define-motion cogent/evil-next-flymake-error (count)
    "Go to the COUNT'th flymake error succeeding point."
    :jump t
    (flymake-goto-next-error (or count 1) nil t))

  (keymap-set evil-motion-state-map "[ Q" 'cogent/evil-prev-flymake-error)
  (keymap-set evil-motion-state-map "] Q" 'cogent/evil-next-flymake-error))

(provide 'cogent-lsp)
