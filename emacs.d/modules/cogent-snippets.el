;;; -*- lexical-binding: t; read-symbol-shorthands: (("cs-" . "cogent-skeleton-")) -*-

(comment
 (use-package yasnippet
   :demand t
   :config
   (yas-global-mode -1)))

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
    "(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:with-user-abort …) :silent t))

(defpackage :foo
  (:use :cl)
  (:export :toplevel *ui*))

(in-package :foo)

;;;; Configuration -----------------------------------------------
(defparameter *whatever* 123)

;;;; Errors ------------------------------------------------------
(define-condition user-error (error) ())

(define-condition missing-foo (user-error) ()
  (:report \"A foo is required, but none was supplied.\"))

;;;; Functionality -----------------------------------------------
(defun foo (string)
  …)

;;;; Run ---------------------------------------------------------
(defun run (arguments)
  (map nil #'foo arguments))

;;;; User Interface ----------------------------------------------
(defmacro exit-on-ctrl-c (&body body)
  `(handler-case (with-user-abort:with-user-abort (progn ,@body))
     (with-user-abort:user-abort () (sb-ext:exit :code 130))))

(defparameter *ui*
  (adopt:make-interface
    :name \"foo\"
    …))

(defun toplevel ()
  (sb-ext:disable-debugger)
  (exit-on-ctrl-c
    (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui*)
      … ; Handle options.
      (handler-case (run arguments)
        (user-error (e) (adopt:print-error-and-exit e))))))")
  (define-abbrev lisp-mode-abbrev-table "cliskel" ""
    'cs-lisp-cli-program))

(provide 'cogent-snippets)
