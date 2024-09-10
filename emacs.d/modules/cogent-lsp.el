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

(provide 'cogent-lsp)
