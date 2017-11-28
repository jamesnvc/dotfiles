;;; -*- lexical-binding: t -*-

(defun cogent-fonts/spec-to-list (spec)
  (s-split "-" spec))

(defun cogent-fonts/list-to-spec (spec)
  (s-join "-" spec))

(defun cogent-fonts/update-font-spec-size (spec increment)
  (cogent-fonts/list-to-spec
   (-update-at 7 (lambda (i) (number-to-string (+ (string-to-number i) increment)))
               (cogent-fonts/spec-to-list spec))))

(defun cogent-fonts/update-font-size (increment)
  (set-frame-font
   (cogent-fonts/update-font-spec-size (frame-parameter nil 'font) increment)))

(provide 'cogent-fonts)
