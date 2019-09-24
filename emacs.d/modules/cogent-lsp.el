;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package lsp-mode
  ;; un-comment this to make the various other lsp-backends get loaded
  ;; :demand t
  :commands lsp
  :hook (prolog-mode . lsp)
  :config

  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-stdio-connection (list "swipl"
                                "-g" "use_module(library(lsp_server))."
                                "-g" "lsp_server:main"
                                "-t" "halt"
                                "--" "stdio"))
    :major-modes '(prolog-mode)
    :priority 1
    :multi-root t
    :server-id 'prolog-ls))
  (add-to-list 'lsp-language-id-configuration '(prolog-mode . "prolog"))

  ;; SH
  ;; to install the client:
  ;; npm i -g bash-language-server

  ;; Rust
  ;; to install the client:
  ;; rustup component add rls-preview rust-analysis rust-src
  ;; (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))

  ;; Ruby
  ;; gem install solargraph
  )

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (general-define-key :keymaps '(lsp-ui-mode-map)
                      [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
                      [remap xref-find-references] #'lsp-ui-peek-find-references)
  (add-hook 'lsp-ui-mode-hook
            (lambda ()
              (lsp-ui-doc-mode -1)
              (setq-local evil-lookup-func #'lsp-ui-doc-show))))

(use-package company-lsp
  :after lsp-mode
  :commands company-lsp
  :init (push 'company-lsp company-backends))

;; C/C++/Objective-C
;; to install the client
;; https://github.com/MaskRay/ccls/wiki/Getting-started
(use-package ccls
  :after lsp-mode
  :config
  (if-let ((ccls-path (cogent/resolve-exec "ccls")))
      (setq ccls-executable ccls-path)
    (setq ccls-executable (expand-file-name "~/software/ccls/Release/ccls")))
  :hook ((objc-mode c++-mode c-mode) . (lambda () (require 'ccls) (lsp))))

(provide 'cogent-lsp)
