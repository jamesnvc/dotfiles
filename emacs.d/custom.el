(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#263238"))
 '(clojure-indent-style :align-arguments)
 '(custom-safe-themes
   (quote
    ("ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" default)))
 '(eshell-visual-commands
   (quote
    ("htop" "vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm" "psql")))
 '(eshell-visual-subcommands (quote (("git " "log" "lol" "diff" "show"))))
 '(helm-ag-base-command "/usr/local/bin/ag --nocolor --nogroup")
 '(evil-search-module (quote evil-search))
 '(fci-rule-color "#37474f")
 '(hl-sexp-background-color "#1c1f26")
 '(menu-bar-mode t)
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "new" :query "tag:inbox and tag:unread")
     (:name "flagged" :query "tag:flagged"))))
 '(org-capture-templates
   (quote
    (("n" "Note" entry
      (file "~/org/notes.org")
      "* %? %U")
     ("w" "Bloom work tasks" entry
      (file "~/org/bloom.org")
      "* %?
Entered on %U
   %a"))))
 '(package-selected-packages
   (quote
    (switch-window haskell-mode dracula-theme centered-window-mode quack geiser web-mode web volatile-highlights use-package twittering-mode tern swiper-helm spaceline smooth-scrolling shrink-whitespace rainbow-mode paradox org-plus-contrib org-cliplink org-bullets nyan-mode monroe material-theme markdown-mode linum-relative json-mode js2-refactor ibuffer-projectile hlinum highlight-parentheses helm-projectile helm-flycheck helm-flx helm-ext helm-cider helm-ag git-gutter-fringe+ git-gutter gist flycheck-color-mode-line f evil-surround evil-search-highlight-persist evil-org evil-nerd-commenter evil-mc evil-magit evil-leader ethan-wspace eshell-git-prompt eros elfeed-org elfeed-goodies dired+ company-try-hard company-quickhelp company-emoji cljr-helm alchemist)))
 '(powerline-default-separator (quote bar))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist (quote (("." . "/home/james/.emacs.d/undo"))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(spaceline-evil-visual ((t (:inherit mode-line :background "dark slate blue" :foreground "#ffffff")))))
