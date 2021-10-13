;;; -*- lexical-binding: t -*-

(require 'cogent-package)
(require 'cogent-base)
(require 'cogent-json)

(use-package rjsx-mode
  :defer t)

(use-package tern
  :commands tern-mode
  :config
  (setq tern-command (list (or (executable-find "tern") "tern"))))

(with-eval-after-load 'flycheck
  (setq flycheck-jshintrc (concat dotfiles-dir "jshintrc")))

(use-package typescript-mode
  :commands typescript-mode)

;;; autoformat stuff - from https://github.com/munen/emacs.d/#auto-formatting
(defun autoformat-with (strategy)
  "Automatically format current buffer using STRATEGY."
  (let ((p (point))
        (s (window-start)))
    ;; Remember the current position
    (save-mark-and-excursion
      ;; Call prettier-eslint binary with the contents of the current
      ;; buffer
      (shell-command-on-region
       (point-min) (point-max)
       (funcall strategy)
       ;; Write into a temporary buffer
       (get-buffer-create "*Temp autoformat buffer*")
       ;; Replace the current buffer with the output of
       ;; the =autoformat strategy= output
       t
       ;; If the =autoformat strategy= returns an error, show it in a
       ;; separate error buffer
       (get-buffer-create "*replace-errors*")
       ;; Automatically show error buffer
       t))
    ;; Return to the previous point and scrolling position (the point
    ;; was lost, because the whole buffer got replaced.
    (set-window-start (selected-window) s)
    (goto-char p)))

(defun autoformat-javascript-command ()
  "CLI tool to format Javascript."
  "prettier --stdin --parser babel")

(defun autoformat-html-command ()
  "CLI tool to format HTML."
  "prettier --stdin --parser html")

(defun autoformat-css-command ()
  "CLI tool to format CSS."
  "prettier --stdin --parser css")

(defun autoformat-sass-command ()
  "CLI tool to format SASS."
  "prettier --stdin --parser sass")

(defun autoformat-json-command ()
  "CLI tool to format JSON."
  "prettier --stdin --parser json")

(defun autoformat-prettier-eslint-command ()
  "CLI tool to format Javascript with .eslintrc.json configuration."
  (concat "prettier-eslint --eslint-config-path "
          ;; Hand over the path of the current projec
          (concat
           (project-root (project-current))
           ".eslintrc.json")
          " --parser babel --stdin"))

(defun autoformat ()
  "Automatically format current buffer."
  (interactive)
  (let ((eslint-path (concat (project-root (project-current))
                             ".eslintrc.json")))
    (autoformat-with
     (cond ((derived-mode-p 'web-mode) 'autoformat-html-command)
           ((derived-mode-p 'css-mode) 'autoformat-css-command)
           ((derived-mode-p 'json-mode) 'autoformat-json-command)
           ((derived-mode-p 'sass-mode) 'autoformat-sass-command)
           ;; JS projects with eslint config
           ((and (file-exists-p eslint-path)
                 (derived-mode-p 'js-mode))
            'autoformat-prettier-eslint-command)
           ((derived-mode-p 'js-mode) 'autoformat-javascript-command)))))

(general-def
  :states '(normal)
  :keymaps '(js-mode-map css-mode-map)
  "<SPC> f" #'autoformat)

(provide 'cogent-javascript)
