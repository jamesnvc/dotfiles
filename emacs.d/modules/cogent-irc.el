;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package erc-hl-nicks
  :commands erc-hl-nicks-enable
  :hook (erc-mode-hook . erc-hl-nicks-enable))

(use-package erc-image)

(use-package erc
  :elpaca nil
  :custom
  ((erc-autojoin-timing 'ident)
   (erc-join-buffer 'bury)
   (erc-lurker-hide-list '("JOIN" "PART" "QUIT" "MODE"))
   (erc-modules
    '(hl-nicks autojoin button completion fill
               irccontrols list match menu move-to-prompt
               netsplit networks noncommands notifications
               readonly ring stamp spelling track image))
   (erc-track-exclude-types
    '("JOIN" "NICK" "PART" "QUIT" "MODE" "333" "353"))))

(with-eval-after-load 'erc
  (add-hook 'window-configuration-change-hook
            (lambda () (setq erc-fill-column (- (window-width) 2)))))

(provide 'cogent-irc)
