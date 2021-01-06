;;; -*- lexical-binding: t -*-

(require 'a)

(defun cogent-fonts/spec-to-list (spec)
  (split-string spec "-"))

(defun cogent-fonts/list-to-spec (spec)
  (string-join spec "-"))

(defun cogent-fonts/update-font-spec-size (spec increment)
  (cogent-fonts/list-to-spec
   (a-update 7 (lambda (i) (number-to-string (+ (string-to-number i) increment)))
             (cogent-fonts/spec-to-list spec))))

(defun cogent-fonts/update-font-size (increment)
  (set-frame-font
   (cogent-fonts/update-font-spec-size (frame-parameter nil 'font) increment)))

(provide 'cogent-fonts)
