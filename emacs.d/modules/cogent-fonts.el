;;; -*- lexical-binding: t -*-

(require 'a)

(defun cogent-fonts/spec-to-list (spec)
  (split-string spec "-"))

(defun cogent-fonts/list-to-spec (spec)
  (string-join spec "-"))

(defun cogent-fonts/update-font-spec-size (spec increment)
  (let ((spec-list (cogent-fonts/spec-to-list spec)))
    (setf (nth 7 spec-list)
          (number-to-string (+ (string-to-number (nth 7 spec-list)) increment)))))

(defun cogent-fonts/update-font-size (increment)
  (set-frame-font
   (cogent-fonts/update-font-spec-size (frame-parameter nil 'font) increment)))

(provide 'cogent-fonts)
