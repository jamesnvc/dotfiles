;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package elixir-mode
  :commands elixir-mode
  :config
  (add-hook 'elixir-mode-hook 'alchemist-mode))

;; Intergration with mix
(use-package alchemist
  :commands alchemist-mode)

(with-eval-after-load "flycheck"
  (flycheck-define-checker
   elixir-mix
   "An Elixir syntax checker using the Elixir interpreter.
See URL `http://elixir-lang.org`.'"
   :command ("mix" "compile" source)
   :error-patterns
   ((error line-start "** (" (zero-or-more not-newline) ") "
           (zero-or-more not-newline) ":" line ": " (message) line-end)
    (warning line-start
             (one-or-more (not (syntax whitespace))) ":"
             line ": "
             (message)
             line-end))
   :modes elixir-mode)
  (add-to-list 'flycheck-checkers 'elixir-mix))

(provide 'cogent-elixir)
