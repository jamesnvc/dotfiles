;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(require 'subr-x)

(defun cogent/font-lock-replace-symbol (mode reg sym)
  "Given a major mode `mode', replace the regular expression `reg'
with the symbol `sym' when rendering"
  (font-lock-add-keywords
   mode `((,reg
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     ,sym 'decompose-region)))))))

;; Running stuff
(defun cogent/exec (command)
  "Run a shell command and return its output as a string, with whitespace trimmed."
  (string-trim (shell-command-to-string command)))

(defun cogent/exec-with-rc (command &rest args)
  "Run a shell command and return a list containing two values: its
return code and its whitespace trimmed output."
  (with-temp-buffer
    (list (apply 'call-process command nil (current-buffer) nil args)
          (string-trim (buffer-string)))))

(defun cogent/is-exec (command)
  "Return true if `command' is an executable on the system search path."
  (file-executable-p
   (string-trim
    (shell-command-to-string (concat "which " command)))))

(defun cogent/resolve-exec (command)
  "If `command' is an executable on the system search path, return its absolute path.
Otherwise, return nil."
  (let ((path (string-trim (shell-command-to-string (concat "which " command)))))
    (when (file-executable-p path) path)))

(defun cogent/exec-if-exec (command args)
  "If `command' satisfies `cogent/is-exec', run it with `args' and
return its output as per `cogent/exec'. Otherwise, return nil."
  (when (cogent/is-exec command) (cogent/exec (concat command " " args))))

;; About the user

(defun cogent/getent (user)
  "Get the /etc/passwd entry for the user `user' as a list of strings,
or nil if there is no such user. Empty fields will be represented as nil,
as opposed to empty strings."
  (let ((ent (cogent/exec (concat "getent passwd " user))))
    (when (not (string-blank-p ent))
      (mapcar (lambda (i) (if (string-blank-p i) nil i))
              (split-string ent ":")))))

(defun cogent/user-full-name ()
  "Guess the user's full name. Return nil if no likely candidate found."
  (or (cogent/exec-if-exec "git" "config --get user.name")
      (elt (cogent/getent (getenv "USER")) 4)))

(defun cogent/user-email ()
  "Guess the user's email or nil if no likely candidate found."
  (or (cogent/exec-if-exec "git" "config --get user.email")
      (getenv "EMAIL")))

(provide 'cogent-base)
