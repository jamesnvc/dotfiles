;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package erc-hl-nicks
  :commands erc-hl-nicks-enable
  :hook (erc-mode . erc-hl-nicks-enable))

(setq erc-network-hide-list '(("localhost:6667" "QUIT" "JOIN" "MODE" "PART")))

(provide 'cogent-irc)
