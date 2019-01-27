;;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defun online? ()
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (cl-some (lambda (iface)
                 (unless (equal "lo" (car iface))
                   (member 'up (car (last (network-interface-info
                                           (car iface)))))))
               (network-interface-list))
    t))

;; Set up straight.el for packages
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer (url-retrieve-synchronously
                          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                          'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(use-package el-patch)

(provide 'cogent-package)
