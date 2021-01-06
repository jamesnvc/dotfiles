;;; -*- lexical-binding: t -*-

(autoload 'ispell-get-word "ispell")

(defun cogent/lookup-word (word)
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (save-excursion (car (ispell-get-word nil))))))
  (if-let ((dict (executable-find "dict")))
      (let ((buf (get-buffer-create "*Help Definition*")))
        (with-current-buffer buf (delete-region (point-min) (point-max)))
        ;; [TODO] allow chosing other dictionaries
        (call-process dict nil buf nil word)
        (display-buffer buf 'display-buffer-pop-up-window))
    (url-retrieve
     (format "https://en.wiktionary.org/wiki/%s" word)
     (lambda (_status)
       (let ((content (thread-first
                          (libxml-parse-html-region (point-min) (point-max))
                        (dom-by-id "mw-content-text")))
             (buf (get-buffer-create "*Help Definition*")))
         (with-current-buffer buf
           (delete-region (point-min) (point-max))
           (insert (dom-texts (car (dom-by-tag content 'p))))
           (insert "\n\n")
           (insert (dom-texts (car (dom-by-tag content 'ol)))))
         (display-buffer buf 'display-buffer-popup-window))))))

(provide 'cogent-writing)
