;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package eglot
  :straight (:type built-in)
  :config
  (add-to-list 'eglot-server-programs (cons 'prolog-mode
                                            (list "swipl"
                                                  ;; "-O"
                                                  ;; "-g" "use_module(library(lsp_server))."
                                                  "-s" (expand-file-name "~/Projects/prolog-lsp/prolog/lsp_server.pl")
                                                  "-g" "lsp_server:main"
                                                  "-t" "halt"
                                                  "--" ;"stdio"
                                                  "port" :autoport)))
  (when (eq system-type 'darwin)
    ;; (setq eglot-server-programs (cdr eglot-server-programs))
    ;; to make sourcekit work properly with xcode project
    ;; install xcode-build-server (brew install xcode-build-server)
    ;; and in the project root run 'xcode-build-server config -project *.xcodeproj'
    (add-to-list 'eglot-server-programs
                 '(swift-mode . ("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"
                                 "-Xswiftc" "-sdk"
                                 "-Xswiftc"
                                 "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk"
                                 "-Xswiftc" "-target"
                                 "-Xswiftc" "arm64-apple-ios18.1-simulator"
                                 "-Xcc" "-DSWIFT_PACKAGE=0"))))
  (add-to-list 'eglot-server-programs
               '((python-ts-mode python-mode) . ("uvx" "pyrefly" "lsp")))
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) . ("~/.rbenv/shims/ruby-lsp")))
  (keymap-set eglot-mode-map "C-c C-a" #'eglot-code-actions)
  (keymap-set eglot-mode-map "C-c C-r" #'eglot-rename)
  (keymap-set eglot-mode-map "C-c C-f" #'eglot-format))

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

(comment
 ;; Replace this with the path to your metta-wam directory
 (let ((mettalog-dir "/path/to/metta-wam"))
   (lsp-register-client
    (make-lsp-client
     :new-connection (lsp-tcp-connection (lambda (port)
                                           (list
                                            "swipl"
                                            "-l" (concat mettalog-dir "/libraries/lsp_server_metta/prolog/lsp_server_metta.pl")
                                            "-g" "lsp_server_metta:main"
                                            "-t" "halt"
                                            "--"
                                            "port" port)))
     :environment-fn (lambda ()
                       (list ("METTALOG_DIR" . mettalog-dir)
                             ("SWIPL_PACK_PATH". (concat mettalog-dir "/libraries"))))
     :major-modes '(metta-mode)
     :activation-fn (lsp-activate-on "metta")
     :server-id 'metta-lsp)))
 )
