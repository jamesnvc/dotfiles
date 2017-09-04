(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clojure-indent-style :align-arguments)
 '(custom-safe-themes
   (quote
    ("98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" default)))
 '(eshell-visual-commands
   (quote
    ("htop" "vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm")))
 '(eshell-visual-subcommands (quote (("git " "log" "lol" "diff" "show"))))
 '(helm-ag-base-command "/usr/local/bin/ag --nocolor --nogroup")
 '(menu-bar-mode nil)
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "new" :query "tag:inbox and tag:unread"))))
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
    (elfeed-goodies elfeed spaceline-all-the-icons web-mode volatile-highlights use-package twittering-mode tern swiper-helm spaceline smooth-scrolling shrink-whitespace rainbow-mode paradox org-plus-contrib org-cliplink org-bullets nyan-mode material-theme markdown-mode linum-relative json-mode js2-refactor ibuffer-projectile hlinum highlight-parentheses helm-projectile helm-flycheck helm-flx helm-ext helm-cider helm-ag git-gutter-fringe+ gist flycheck-color-mode-line f evil-surround evil-search-highlight-persist evil-org evil-nerd-commenter evil-mc evil-magit evil-leader ethan-wspace eshell-git-prompt eros dired+ company-try-hard company-quickhelp company-emoji cljr-helm alchemist)))
 '(powerline-default-separator (quote bar))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist (quote (("." . "/Users/james/.emacs.d/undo")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#263238"))))
 '(mode-line ((t (:background "#1c1f26" :foreground "#ffffff" :family "PragmataPro"))))
 '(mode-line-highlight ((t (:foreground "#b39ddb" :box nil))))
 '(spaceline-evil-visual ((t (:inherit mode-line :background "dark slate blue" :foreground "#ffffff")))))
