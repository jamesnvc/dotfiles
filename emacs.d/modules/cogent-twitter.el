;;; -*- lexical-binding: t -*-

(use-package twittering-mode
  :config
  (setq twittering-use-master-password t)
  :bind (:map twittering-mode-map
              ("*" . twittering-favorite)))

(provide 'cogent-twitter)
