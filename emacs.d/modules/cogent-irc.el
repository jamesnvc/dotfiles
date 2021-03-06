;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package erc-hl-nicks
  :commands erc-hl-nicks-enable
  :hook (erc-mode-hook . erc-hl-nicks-enable))

(use-package erc-image)

(with-eval-after-load 'erc
  (add-hook 'window-configuration-change-hook
            (lambda () (setq erc-fill-column (- (window-width) 2)))))

(provide 'cogent-irc)
