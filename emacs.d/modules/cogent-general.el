;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(defvar cogent/exec-path-initialized nil)

(defvar cogent/exec-path-after-inited nil)

(defmacro cogent/after-path-init (&rest body)
  "Make sure `body' is run after the path is inited from shell env."
  `(if cogent/exec-path-initialized
       (progn ,@body)
     (push (lambda () (progn ,@body)) cogent/exec-path-after-inited)))

(defun cogent/path-inited ()
  (dolist (hook cogent/exec-path-after-inited)
    (funcall hook))
  (setq cogent/exec-path-after-inited nil))

(use-package exec-path-from-shell
  :defer 2
  :commands exec-path-from-shell-initialize
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  (unless (getenv "NIX_PATH")
    (exec-path-from-shell-copy-envs
     '("NIX_PATH" "NIX_PROFILES" "NIX_SSL_CERT_FILE")))
  (setq cogent/exec-path-initialized t)
  (cogent/path-inited))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(require 'iso-transl)

(setq backup-directory-alist `(("." . ,(expand-file-name (concat dotfiles-dir "bak")))))

(setq compilation-ask-about-save nil)

(delete-selection-mode t)
(transient-mark-mode t)

(global-auto-revert-mode 1)

(setq-default
 browse-url-browser-function (quote browse-url-generic)
 browse-url-generic-program (if (string-equal system-type "darwin")
                                "open"
                              "xdg-open"))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(provide 'cogent-general)
