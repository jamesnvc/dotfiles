;;; -*- lexical-binding: t -*-

(use-package dart-mode
  :config
  ;; (add-hook 'dart-mode-hook #'lsp)
  )

(use-package flutter
  :after dart
  :config
  (define-key dart-mode-map (kbd "C-M-x") #'flutter-run-or-hot-reload)
  (define-key dart-mode-map (kbd "C-c C-z") (lambda () (interactive) (display-buffer "*Flutter*"))))

(use-package lsp-dart
  :after (dart lsp)
  :config
  (setq lsp-dart-sdk-dir
        (concat
         (cogent/exec "flutter sdk-path")
         "/bin/cache/dart-sdk"))
  (setq lsp-dart-flutter-sdk-dir (cogent/exec "flutter sdk-path")))

(provide 'cogent-flutter)
