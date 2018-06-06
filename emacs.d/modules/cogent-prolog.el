;;; -*- lexical-binding: t -*-

(use-package mmm-mode
  :config
  (setq mmm-global-mode 'maybe)
  (mmm-add-classes
   '((prolog-javascript
      ;; would prefer js2-mode, but it explicitly doesn't do syntax
      ;; highlighting in mmm-mode
      :submode javascript-mode
      :front "{|javascript([^)]*)||[\n\r]+"
      :back "|}")))
  (mmm-add-mode-ext-class 'prolog-mode nil 'prolog-javascript))

(provide 'cogent-prolog)
