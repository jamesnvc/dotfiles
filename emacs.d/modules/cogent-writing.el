;;; -*- lexical-binding: t -*-

(autoload 'ispell-get-word "ispell")

(with-eval-after-load 'ispell
  (setf (alist-get nil ispell-dictionary-alist)
        '("[[:alpha:]]" "[^[:alpha:]]" "[0-9']" t ("-d" "en_CA") nil utf-8)))

(defun cogent/lookup-word (word)
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (save-excursion (car (ispell-get-word nil))))))
  (cond
   ((fboundp 'dictionary-search) (dictionary-search word))
   ((dict (executable-find "dict"))
    (let ((buf (get-buffer-create "*Help Definition*")))
      (with-current-buffer buf (delete-region (point-min) (point-max)))
      ;; [TODO] allow choosing other dictionaries
      (call-process dict nil buf nil word)
      (display-buffer buf 'display-buffer-pop-up-window)))
   (t
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
         (display-buffer buf 'display-buffer-popup-window)))))))

(defun cogent/synonyms (word)
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (save-excursion (car (ispell-get-word nil))))))
    (cl-assert (executable-find "wn") "Install WordNet to look up synonyms")
    (let ((buf (get-buffer-create "*Synonyms*")))
      (with-current-buffer buf
        (view-mode -1)
        (delete-region (point-min) (point-max))
        (insert "Synonyms of '" word "'")
        (add-text-properties (point-min) (point-max) '(face bold))
        (insert "\n\n"))
      (call-process (executable-find "wn")
                    nil buf nil
                    word "-synsa")
      (with-current-buffer buf (view-mode +1))
      (display-buffer buf 'display-buffer-popup-window)))

(defun cogent/lookup-etymology (word)
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (save-excursion (car (ispell-get-word nil))))))
  ;; [TODO]
  )

(use-package dictionary
  :straight (:type built-in)
  :config
  (setopt dictionary-search-strategy "prefix"
          dictionary-server "dict.org"))

(provide 'cogent-writing)
