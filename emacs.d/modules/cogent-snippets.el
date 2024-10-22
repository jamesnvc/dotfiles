;;; -*- lexical-binding: t; read-symbol-shorthands: (("cs-" . "cogent-skeleton-")) -*-

;; eglot uses this
(use-package yasnippet
  :demand t)

(use-package skeleton
  :straight (:type built-in)
  :demand t

  :config

  (setopt save-abbrevs nil) ; don't save abbrevs to file, this defines them

  (define-skeleton cs-uuid
    "Insert a randomly-generated uuid"
    nil
    (cogent/exec "uuidgen | tr '[A-Z]' '[a-z]' | tr -d '\n'"))
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
  (with-eval-after-load 'sweeprolog
    (define-abbrev sweeprolog-mode-abbrev-table "mod" "" 'cs-prolog-module))

  (define-skeleton cs-prolog-module-comment
    "Insert a prolog module documentation comment"
    nil
    "/** <module> " _ "\n"
    "\n"
    "@author James Cash\n"
    "*/")
  (define-abbrev prolog-mode-abbrev-table "moddoc" ""
    'cs-prolog-module-comment)
  (with-eval-after-load 'sweeprolog
    (define-abbrev sweeprolog-mode-abbrev-table "moddoc" ""
      'cs-prolog-module-comment))

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
  (with-eval-after-load 'sweeprolog
    (define-abbrev sweeprolog-mode-abbrev-table "plunit" ""
      'cs-prolog-plunit))

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
    (when v2 '("Predicate: " str ", "))
    (when v2 -2) ; delete last ', '
    (when v2 "]")
    ").")
  (define-abbrev prolog-mode-abbrev-table "use" ""
    'cs-prolog-use-module)
  (with-eval-after-load 'sweeprolog
    (define-abbrev sweeprolog-mode-abbrev-table "use" ""
      'cs-prolog-use-module))


  (define-skeleton cs-lisp-cli-program
    ""
    "Basic lisp program"
    nil
    "(eval-when (:compile-toplevel :load-toplevel :execute)\n"
    "  (ql:quickload '(:with-user-abort :adopt) :silent t))\n"
    "\n"
    "(defpackage :" str | (file-name-base (buffer-file-name)) "\n"
    "  (:use :cl)\n"
    "  (:export :toplevel *ui*))\n"
    "\n"
    "(in-package :" str | (file-name-base (buffer-file-name)) ")\n"
    "\n"
    ";;;; Configuration -----------------------------------------------\n"
    ";; (defparameter *whatever* 123)\n"
    "\n"
    ";;;; Errors ------------------------------------------------------\n"
    "(define-condition user-error (error) ())\n"
    "\n"
    "(define-condition missing-foo (user-error) ()\n"
    "  (:report \"A foo is required, but none was supplied.\"))\n"
    "\n"
    "(define-condition too-many-arguments (user-error) ()\n"
    "  (:report \"Too many arguments, see usage\"))\n"
    "\n"
    ";;;; Functionality -----------------------------------------------\n"
    "\n"
    "(defun do-stuff ()\n"
    "   )\n"
    "\n"
    ";;;; Run ---------------------------------------------------------\n"
    "\n"
    "(defun run (arguments)\n"
    "  (map 'nil #'do-stuff arguments)\n"
    "\n"
    ";;;; User Interface ----------------------------------------------\n"
    "(defmacro exit-on-ctrl-c (&body body)\n"
    "  `(handler-case (with-user-abort:with-user-abort (progn ,@body))\n"
    "     (with-user-abort:user-abort () (sb-ext:exit :code 130))))\n"
    "\n"
    "(defparameter *option-help*\n"
    "  (adopt:make-option 'help\n"
    "    :help \"Display help and exit.\"\n"
    "    :long \"help\"\n"
    "    :short #\h\n"
    "    :reduce (constantly t)))\n"
    "\n"
    "(adopt:defparameters (*option-debug* *option-no-debug*)\n"
    "  (adopt:make-boolean-options 'debug\n"
    "    :long \"debug\"\n"
    "    :short #\d\n"
    "    :help \"Enable the Lisp debugger.\"\n"
    "    :help-no \"Disable the Lisp debugger (the default).\"))\n"
    "\n"
    "(defparameter *ui*\n"
    "  (adopt:make-interface\n"
    "    :name \"foo\"\n"
    "    :summary \"do something neat\"\n"
    "    :usage \"[OPTIONS] ARGS\"\n"
    "    :help \"Do something interesting\"\n"
    "    :contents (list *option-help* *option-debug*)))\n"
    "\n"
    "(defun toplevel ()\n"
    "  (sb-ext:disable-debugger)\n"
    "  (exit-on-ctrl-c\n"
    "    (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui*)\n"
    "      (when (gethash 'debug options)\n"
    "        (sb-ext:enable-debugger))\n"
    "      (handler-case\n"
    "          (cond\n"
    "            ((gethash 'help options) (adopt:print-help-and-exit *ui*))\n"
    "            ((null arguments) (error 'missing-foo))\n"
    "            ((> (length arguments) 2) (error 'too-many-arguments))\n"
    "            (t (run arguments)))\n"
    "        (user-error (e) (adopt:print-error-and-exit e))))))\n")
  (define-abbrev lisp-mode-abbrev-table "cliskel" ""
    'cs-lisp-cli-program))

(provide 'cogent-snippets)
