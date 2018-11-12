;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package lsp-mode
  ;; :demand t
  :commands lsp-define-stdio-client lsp-mode lsp-sh-enable
  :config
  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)

  (require 'cogent-complete)


  ;; SH
  ;; to install the client:
  ;; npm i -g bash-language-server
  (lsp-define-stdio-client
   lsp-sh "sh"
   #'(lambda () default-directory)
   `(,(let ((npm-prefix (s-chomp (shell-command-to-string "npm config get prefix"))))
        (s-append "/bin/bash-language-server" npm-prefix))
     "start"))
  ;; not enabling it by default - kinda overkill
  ;; (add-hook 'sh-mode-hook #'lsp-sh-enable)
  )

(use-package lsp-ui
  :after lsp-mode
  :config
  (add-hook 'lsp-mode-hook #'lsp-ui-mode))

(use-package company-lsp
  :after company lsp-mode
  :demand t
  :init (push 'company-lsp company-backends))

;; Rust
;; to install the client:
;; rustup component add rls-preview rust-analysis rust-src
(use-package lsp-rust
  :after lsp-mode rust-mode
  :commands lsp-rust-enable
  :init
  (add-hook 'rust-mode-hook #'lsp-rust-enable)
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls")))

;; C/C++/Objective-C
;; to install the client
;; https://github.com/MaskRay/ccls/wiki/Getting-started
(use-package ccls
  :after lsp-mode
  :commands lsp-ccls-enable
  :config
  (if-let ((ccls-path (cogent/resolve-exec "ccls")))
      (setq ccls-executable ccls-path)
    (setq ccls-executable (expand-file-name "~/software/ccls/Release/ccls")))
  :init
  (add-hook 'objc-mode-hook #'lsp-ccls-enable))

(provide 'cogent-lsp)
