;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(setq lsp-use-plists t)
(use-package lsp-mode
  ;; un-comment this to make the various other lsp-backends get loaded
  ;; :demand t
  :commands lsp
  :hook (prolog-mode-hook . lsp)
  :config

  (setq read-process-output-max (* 1024 1024))
  (customize-set-variable 'lsp-idle-delay 1.0)

  ;; (add-to-list 'lsp-semantic-token-faces
  ;;              (cons "modifier"
  ;;                    'lsp-face-semhl-property))

  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-stdio-connection (list "swipl"
                                "-O"
                                ;; "-g" "use_module(library(lsp_server))."
                                "-s" (expand-file-name "~/Projects/prolog-lsp/prolog/lsp_server.pl")
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

(use-package posframe)

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode-hook . lsp-ui-mode)
  :config
  (general-define-key :keymaps '(lsp-ui-mode-map)
                      [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
                      [remap xref-find-references] #'lsp-ui-peek-find-references)
  (add-hook 'lsp-ui-mode-hook
            (lambda ()
              (lsp-ui-doc-mode -1)
              (setq-local evil-lookup-func #'lsp-ui-doc-show)))
  ;; make peeks appear in a child frame
  ;; https://github.com/emacs-lsp/lsp-ui/issues/441
  (defun lsp-ui-peek--peek-display (src1 src2)
    (-let* ((win-width (frame-width))
            (lsp-ui-peek-list-width (/ (frame-width) 2))
            (string (-some--> (-zip-fill "" src1 src2)
                      (--map (lsp-ui-peek--adjust win-width it) it)
                      (-map-indexed 'lsp-ui-peek--make-line it)
                      (-concat it (lsp-ui-peek--make-footer)))))
      (setq lsp-ui-peek--buffer (get-buffer-create "*lsp-peek--buffer*"))
      (posframe-show lsp-ui-peek--buffer
                     :string (mapconcat 'identity string "")
                     :min-width (frame-width)
                     :poshandler #'posframe-poshandler-frame-center)))

  (defun lsp-ui-peek--peek-destroy ()
    (when (and (boundp 'lsp-ui-peek--buffer)
               (bufferp lsp-ui-peek--buffer))
      (posframe-delete lsp-ui-peek--buffer))
    (setq lsp-ui-peek--buffer nil
          lsp-ui-peek--last-xref nil)
    (set-window-start (get-buffer-window) lsp-ui-peek--win-start))

  (advice-add #'lsp-ui-peek--peek-new :override #'lsp-ui-peek--peek-display)
  (advice-add #'lsp-ui-peek--peek-hide :override #'lsp-ui-peek--peek-destroy))

;; C/C++/Objective-C
;; to install the client
;; https://github.com/MaskRay/ccls/wiki/Getting-started
(use-package ccls
  :after lsp-mode
  :config
  (if-let ((ccls-path (executable-find "ccls")))
      (setq ccls-executable ccls-path)
    (setq ccls-executable (expand-file-name "~/software/ccls/Release/ccls"))))

(use-package dap-mode
  :commands dap-mode
  :config
  (require 'dap-swi-prolog)
  (dap-register-debug-template
   "SWI-Prolog Start Terminal"
   (list :type "swi-prolog"
         :goal "$run_in_terminal"
         :request "launch"
         :name "SWI-Prolog::Terminal")))

(provide 'cogent-lsp)
