;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package lsp-mode
  :commands lsp-define-stdio-client lsp-mode
  :init
  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)
  (use-package lsp-ui
    :config
    (add-hook 'lsp-mode-hook #'lsp-ui-mode))
  (require 'cogent-complete)
  (use-package company-lsp
    :demand t
    :init (push 'company-lsp company-backends))

  :config
  ;; backends

  ;; SH
  ;; to install the client:
  ;; npm i -g bash-language-server
  (lsp-define-stdio-client
   lsp-sh "sh"
   #'(lambda () default-directory)
   `(,(let ((npm-prefix (s-chomp (shell-command-to-string "npm config get prefix")))
            )
        (s-append "/bin/bash-language-server" npm-prefix))
     "start"))
  (add-hook 'sh-mode #'lsp-sh-enable)


  ;; Rust
  ;; to install the client:
  ;; rustup component add rls-preview rust-analysis rust-src
  (use-package lsp-rust
    :commands lsp-rust-enable
    :init (add-hook 'rust-mode #'lsp-rust-enable))
  )

(provide 'cogent-lsp)
