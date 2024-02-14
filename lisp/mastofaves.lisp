(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:with-user-abort :adopt :drakma :ciao) :silent t))

(defpackage :mastofaves
  (:use :cl)
  (:export :toplevel *ui*))

(in-package :mastofaves)

;;;; Configuration -----------------------------------------------

(defparameter *saved-db-path* #p"~/.local/cache/mastofaves/faves.sqlite")

(defparameter *oauth-keys-path* #p"~/.config/mastofaves/config.sexp")

(defparameter *masto-server-hostname* "https://fosstodon.org")

;;;; Errors ------------------------------------------------------
(define-condition user-error (error) ())

(define-condition missing-auth (user-error) ()
  (:report "Missing OAuth credentials."))

(define-condition too-many-arguments (user-error) ()
  (:report "Too many arguments, see usage"))

;;;; Functionality -----------------------------------------------

(defun load-credentials ()
  (with-open-file (s *oauth-keys-path*)
    (read s)))

(defun oauthorize ()
  (let* ((auth-server (make-instance 'ciao:oauth2-auth-server
                                     :token-url (format nil "~d/oauth/token" *masto-server-hostname*)
                                     :auth-url (format nil "~d/oauth/authorize" *masto-server-hostname*)
                                     :revoke-url (format nil "~d/oauth/revoke" *masto-server-hostname*)))
         (client (make-instance 'ciao:oauth2-client
                               :secret (cdr (assoc :client-secret (load-credentials)))
                               :id (cdr (assoc :client-key (load-credentials)))))
         (auth-url (ciao:get-auth-request-url auth-server
                                              :client client
                                              :scopes '("read:favourites")
                                              :redirect-uri ciao:*oob-uri*)))
    (trivial-open-browser:open-browser
     (quri:render-uri auth-url))
    (let ((auth-code (read-line)))
      (ciao:oauth2/auth-code auth-server client auth-code
                             :redirect-uri ciao:*oob-uri*))))

(defun fix-token-date (oauth-token)
  "Ciao has a bug where it tries to call < on the current time and the expiration time, but when it's +inf, SBCL throws an error"
  (setf (slot-value oauth-token 'ciao::expiration) (+ (get-universal-time) 1000))
  oauth-token)

(defun fetch-favourites (oauth-token)
  (dex:get (format nil "~d/api/v1/favourites" *masto-server-hostname*)
           :headers (ciao:headers oauth-token)))

;;;; Run ---------------------------------------------------------

(defun run (arguments)
  )

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
    :short #\
    :help "Enable the Lisp debugger."
    :help-no "Disable the Lisp debugger (the default)."))

(defparameter *ui*
  (adopt:make-interface
    :name "foo"
    :summary "do something neat"
    :usage "[OPTIONS] ARGS"
    :help "Do something interesting"
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
            ((null arguments) (error 'missing-foo))
            ((> (length arguments) 2) (error 'too-many-arguments))
            (t (run arguments)))
        (user-error (e) (adopt:print-error-and-exit e))))))
