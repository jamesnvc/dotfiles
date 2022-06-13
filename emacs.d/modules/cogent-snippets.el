;;; -*- lexical-binding: t; read-symbol-shorthands: (("cs-" . "cogent-skeleton-")) -*-

(comment
 (use-package yasnippet
   :demand t
   :config
   (yas-global-mode -1)))

(use-package skeleton
  :straight (:type built-in)

  :config
  (define-skeleton cs-uuid
    "Insert a randomly-generated uuid"
    nil
    (cogent/exec "uuidgen"))
  (define-abbrev global-abbrev-table "uuid" "" 'cs-uuid)

  (define-skeleton cs-now
    "Insert a timestamp for `now'"
    nil
    (format-time-string "%Y-%m-%d_%H%M%S"))
  (define-abbrev global-abbrev-table "now" "" 'cs-now)

  (define-skeleton cs-date
    "Insert a timestamp for just the date of today"
    nil
    (format-time-string "%Y-%m-%d"))
  (define-abbrev global-abbrev-table "date" "" 'cs-date)

  (define-skeleton cs-prolog-module
    "Insert a prolog module directive"
    "Module: "
    ":- module(" str | (file-name-base (buffer-file-name)) ", []).\n"
    _)
  (define-abbrev prolog-mode-abbrev-table "mod" "" 'cs-prolog-module)

  (define-skeleton cs-prolog-module-comment
    "Insert a prolog module documentation comment"
    nil
    "/** <module> " _ "\n"
    "\n"
    "@author James Cash\n"
    "*/")
  (define-abbrev prolog-mode-abbrev-table "moddoc" ""
    'cs-prolog-module-comment)

  (define-skeleton cs-prolog-plunit
    "Insert plunit scaffolding"
    "Module: "
    ":- module(" str | (file-name-base (buffer-file-name)) ", []).\n"
    "\n"
    '(setq base-mod (replace-regexp-in-string
                     "_test$" ""
                     (if (string= str "")
                         (file-name-base (buffer-file-name))
                       str)))
    ":- use_module(library(plunit)).\n"
    ":- use_module(" base-mod ").\n"
    "\n"
    ":- begin_tests(" base-mod ").\n"
    "\n"
    _ "\n"
    "\n"
    ":- end_tests(" base-mod ").\n")
  (define-abbrev prolog-mode-abbrev-table "plunit" ""
    'cs-prolog-plunit)

  (define-skeleton cs-prolog-use-module
    "Insert use-module stanza"
    nil
    ":- use_module("
    '(setq v1 (y-or-n-p "Library? "))
    (when v1 "library(")
    (skeleton-read "Module: ")
    (when v1 ")")
    '(setq v2 (y-or-n-p "Import? "))
    (when v2 ", [")
    (when v2 '("Predicate: " str ", ")) -2
    (when v2 "]")
    ").")
  (define-abbrev prolog-mode-abbrev-table "use" ""
    'cs-prolog-use-module))

(provide 'cogent-snippets)
