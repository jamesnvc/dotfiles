(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:with-user-abort :adopt :local-time) :silent t))

(defpackage :datediff
  (:use :cl)
  (:export :toplevel *ui*))

(in-package :datediff)

;;;; Configuration -----------------------------------------------
;; (defparameter *whatever* 123)

;;;; Errors ------------------------------------------------------
(define-condition user-error (error) ())

(define-condition missing-start-date (user-error) ()
  (:report "An initial date is required, but none was supplied."))

(define-condition too-many-arguments (user-error) ()
  (:report "Too many arguments, see usage"))

(define-condition malformed-date (user-error)
  ((date-string :initarg :date-string))
  (:report (lambda (c s)
             (format s "Malformed date ~s" (slot-value c 'date-string)))))

;;;; Functionality -----------------------------------------------
(defun date-delta-days (ts1 ts2)
  "Return the span in days between `ts1' and `ts2'.

   (date-delta-days
     (local-time:parse-timestring \"2024-01-24\")
     (local-time:parse-timestring \"2023-10-16\")) => 100 (7 bits, #x64, #o144, #b1100100)
"
  (abs (- (local-time:day-of ts2)
          (local-time:day-of ts1))))

(defun date-delta-weeks (ts1 ts2)
  (floor (/ (date-delta-days ts1 ts2) 7)))

(defun date-delta-months (ts1 ts2)
  (let ((ts1 (local-time:timestamp-minimum ts1 ts2))
        (ts2 (local-time:timestamp-maximum ts1 ts2)))
    (+ (* 12 (local-time:timestamp-whole-year-difference ts2 ts1))
       (mod (- (local-time:timestamp-month ts2)
               (local-time:timestamp-month ts1))
            12)
       (if (< (local-time:timestamp-day ts2)
              (local-time:timestamp-day ts1))
           -1
           0))))

(defun date-delta-years (ts1 ts2)
  (let ((ts1 (local-time:timestamp-minimum ts1 ts2))
        (ts2 (local-time:timestamp-maximum ts1 ts2)))
    (local-time:timestamp-whole-year-difference ts2 ts1)))

#|
(date-delta-months
 (local-time:parse-timestring "2024-01-24")
 (local-time:parse-timestring "2023-10-16"))

(date-delta-years (local-time:parse-timestring "2024-01-24")
                  (local-time:parse-timestring "2023-10-16"))

(run% (local-time:parse-timestring "2024-01-24")
      (local-time:parse-timestring "2023-10-16"))
|#

;;;; Run ---------------------------------------------------------
(defun run% (ts1 ts2)
  (let ((days (date-delta-days ts1 ts2))
        (weeks (date-delta-weeks ts1 ts2))
        (months (date-delta-months ts1 ts2))
        (years (date-delta-years ts1 ts2)))
    (format t "~&~a Day~:p~%~a Week~:p~%~a Month~:p~%~a Year~:p~%" days weeks months years)))

(defun run (ts1 &optional (ts2 (local-time:now) ts2-given-p))
  (let ((ts1 (handler-case (local-time:parse-timestring ts1)
               (local-time:invalid-timestring ()
                 (error 'malformed-date :date-string ts1))))
        (ts2 (if ts2-given-p
                 (handler-case (local-time:parse-timestring ts2)
                   (local-time:invalid-timestring ()
                     (error 'malformed-date :date-string ts2)))
                 ts2)))
    (run% ts1 ts2)))

;;;; User Interface ----------------------------------------------
(defmacro exit-on-ctrl-c (&body body)
  `(handler-case (with-user-abort:with-user-abort (progn ,@body))
     (with-user-abort:user-abort () (sb-ext:exit :code 130))))

(defparameter *option-help*
  (adopt:make-option 'help
    :help "Display help and exit."
    :long "help"
    :short #\h
    :reduce (constantly t)))

(adopt:defparameters (*option-debug* *option-no-debug*)
  (adopt:make-boolean-options 'debug
    :long "debug"
    :short #\d
    :help "Enable the Lisp debugger."
    :help-no "Disable the Lisp debugger (the default)."))

(defparameter *ui*
  (adopt:make-interface
    :name "datediff"
    :summary "show the delta between two dates"
    :usage "[OPTIONS] DATE1 [DATE2]"
    :help "Show the different between DATE1 and DATE2 in a variety of units."
    :contents (list *option-help* *option-debug*)))

(defun toplevel ()
  (sb-ext:disable-debugger)
  (exit-on-ctrl-c
    (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui*)
      (when (gethash 'debug options)
        (sb-ext:enable-debugger))
      (handler-case
          (cond
            ((gethash 'help options) (adopt:print-help-and-exit *ui*))
            ((null arguments) (error 'missing-start-date))
            ((> (length arguments) 2) (error 'too-many-arguments))
            (t (apply #'run arguments)))
        (user-error (e) (adopt:print-error-and-exit e))))))
