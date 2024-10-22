(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["light gray" "#f36c60" "medium sea green" "dark goldenrod"
    "dark cyan" "dark magenta" "blue" "#263238"])
 '(auth-sources '("~/.authinfo.gpg" "~/.netrc"))
 '(calendar-week-start-day 1)
 '(cider-auto-select-error-buffer nil)
 '(clojure-indent-style :align-arguments)
 '(confirm-kill-emacs 'yes-or-no-p)
 '(custom-safe-themes
   '("9a977ddae55e0e91c09952e96d614ae0be69727ea78ca145beea1aae01ac78d2"
     "b9c804f672050817981dcc58a04e236d685417c3318554088d5552c819243281"
     "82f0f47ac811eeb45fbcfc5fee48989e4d0bca11f74653b838c29fab9a20aee7"
     "e405944a54b99c11463edddeb9ff4f8dd981cd2ae14a4b58458536c451323381"
     "64eff8a4f640f94bb22b05ad3e33888dfa9cab0823b05a3a70b7c133660df748"
     "4ef7fae9161de7ada26d79ac4fb28c83e9c418286a7ed465748efa46d4dc4482"
     "ef78e62377e6ad7b155ea7c6590f3f45e56b26d4cfddff57c81e11974b52c708"
     "046e442b73846ae114d575a51be9edb081a1ef29c05ae5e237d5769ecfd70c2e"
     "ce409d3a932171747b7b8b7edafdc70b4614beb31a7204aa25517d7c0ab80c48"
     "7435c097e5d051fa34ad58bf94ac1b2d5d1e14e62fd20dfedf2bb4403b09a446"
     "8566e9107b01de614891b9b397c6f5b66ce2d328fd8f04770260c58c62c0c2b8"
     "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6"
     default))
 '(dired-guess-shell-alist-user
   '(("\\.wav" (if (string-equal system-type "darwin") "afplay" "aplay"))))
 '(display-line-numbers-type 'relative)
 '(display-raw-bytes-as-hex t)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(elfeed-sort-order 'ascending)
 '(eshell-visual-commands
   '("htop" "vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin"
     "trn" "elm" "psql"))
 '(eshell-visual-subcommands '(("git " "log" "lol" "diff" "show")))
 '(evil-emacs-state-modes
   '(sly-xref-mode sly-db-mode elfeed-show-mode sly-stickers--replay-mode
                   image-mode eww-mode eww-history-mode diff-mode
                   dired-mode Man-mode woman-mode vundo--mode
                   completion-mode process-menu-mode timer-list-mode
                   info-mode grep-mode view-mode sly-trace-dialog-mode
                   sly-apropos-mode sly-apropos-mode
                   sly-trace-dialog-mode cider-stacktrace-mode
                   cider-docview-mode embark-collect-mode view-mode
                   grep-mode info-mode timer-list-mode
                   process-menu-mode completion-mode vundo--mode
                   woman-mode Man-mode dired-mode diff-mode
                   eww-history-mode eww-mode image-mode 5x5-mode
                   archive-mode bbdb-mode biblio-selection-mode
                   blackbox-mode bookmark-bmenu-mode
                   bookmark-edit-annotation-mode browse-kill-ring-mode
                   bs-mode bubbles-mode bzr-annotate-mode calc-mode
                   cfw:calendar-mode completion-list-mode Custom-mode
                   custom-theme-choose-mode debugger-mode
                   delicious-search-mode desktop-menu-blist-mode
                   desktop-menu-mode doc-view-mode dun-mode
                   dvc-bookmarks-mode dvc-diff-mode
                   dvc-info-buffer-mode dvc-log-buffer-mode
                   dvc-revlist-mode dvc-revlog-mode dvc-status-mode
                   dvc-tips-mode ediff-mode ediff-meta-mode efs-mode
                   Electric-buffer-menu-mode emms-browser-mode
                   emms-mark-mode emms-metaplaylist-mode
                   emms-playlist-mode ess-help-mode etags-select-mode
                   fj-mode gc-issues-mode gdb-breakpoints-mode
                   gdb-disassembly-mode gdb-frames-mode
                   gdb-locals-mode gdb-memory-mode gdb-registers-mode
                   gdb-threads-mode gist-list-mode git-rebase-mode
                   gnus-article-mode gnus-browse-mode gnus-group-mode
                   gnus-server-mode gnus-summary-mode gomoku-mode
                   google-maps-static-mode ibuffer-mode
                   jde-javadoc-checker-report-mode magit-cherry-mode
                   magit-diff-mode magit-log-mode
                   magit-log-select-mode magit-popup-mode
                   magit-popup-sequence-mode magit-process-mode
                   magit-reflog-mode magit-refs-mode
                   magit-revision-mode magit-stash-mode
                   magit-stashes-mode magit-status-mode mh-folder-mode
                   monky-mode mpuz-mode mu4e-main-mode
                   mu4e-headers-mode mu4e-view-mode notmuch-hello-mode
                   notmuch-search-mode notmuch-show-mode
                   notmuch-tree-mode occur-mode org-agenda-mode
                   package-menu-mode pdf-outline-buffer-mode
                   pdf-view-mode proced-mode rcirc-mode rebase-mode
                   recentf-dialog-mode reftex-select-bib-mode
                   reftex-select-label-mode reftex-toc-mode sldb-mode
                   slime-inspector-mode slime-thread-control-mode
                   slime-xref-mode snake-mode solitaire-mode
                   sr-buttons-mode sr-mode sr-tree-mode
                   sr-virtual-mode tar-mode tetris-mode
                   tla-annotate-mode tla-archive-list-mode
                   tla-bconfig-mode tla-bookmarks-mode
                   tla-branch-list-mode tla-browse-mode
                   tla-category-list-mode tla-changelog-mode
                   tla-follow-symlinks-mode tla-inventory-file-mode
                   tla-inventory-mode tla-lint-mode tla-logs-mode
                   tla-revision-list-mode tla-revlog-mode
                   tla-tree-lint-mode tla-version-list-mode
                   twittering-mode urlview-mode vc-annotate-mode
                   vc-dir-mode vc-git-log-view-mode
                   vc-hg-log-view-mode vc-svn-log-view-mode vm-mode
                   vm-summary-mode w3m-mode wab-compilation-mode
                   xgit-annotate-mode xgit-changelog-mode
                   xgit-diff-mode xgit-revlog-mode xhg-annotate-mode
                   xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode
                   xhg-status-extra-mode))
 '(evil-surround-pairs-alist
   '((40 "( " . " )") (91 "[ " . " ]") (123 "{ " . " }") (41 "(" . ")")
     (93 "[" . "]") (125 "{" . "}") (35 "#{" . "}") (98 "(" . ")")
     (66 "{" . "}") (62 "<" . ">") (116 . evil-surround-read-tag)
     (60 . evil-surround-read-tag) (102 . evil-surround-function)
     (124 "「" . "」") (107 "【" . "】")))
 '(flycheck-color-mode-line-face-to-color 'cogent-line-buffer-name-face)
 '(flycheck-proselint-executable "~/.pyenv/shims/proselint")
 '(hl-sexp-background-color "#1c1f26")
 '(load-prefer-newer t)
 '(lsp-enable-text-document-color nil)
 '(lsp-semantic-tokens-apply-modifiers t)
 '(lsp-semantic-tokens-enable t)
 '(lsp-ui-doc-border "royal blue")
 '(lsp-ui-doc-header nil)
 '(lsp-ui-doc-max-height 50)
 '(lsp-ui-doc-max-width 80)
 '(lsp-ui-doc-position 'top)
 '(lsp-ui-doc-use-webkit nil)
 '(lsp-ui-peek-always-show nil)
 '(lsp-ui-peek-list-width 50)
 '(lsp-ui-peek-peek-height 10)
 '(lsp-ui-sideline-show-hover nil)
 '(magit-diff-refine-hunk t)
 '(menu-bar-mode t)
 '(mode-require-final-newline nil)
 '(moody-mode-line-height 28)
 '(mouse-drag-and-drop-region 'modifier)
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "new" :query "tag:inbox and tag:unread")
     (:name "flagged" :query "tag:flagged")
     (:name "emacs-devel" :query
            "tag:emacs and tag:list and tag:unread" :key "e")))
 '(notmuch-show-logo nil)
 '(org-babel-clojure-backend 'cider)
 '(org-babel-load-languages '((shell . t) (emacs-lisp . t) (dot . t)))
 '(org-capture-templates
   '(("n" "Note" plain (file cogent/org-note-file-name)
      "#+DATE: %t\12* %^{title}    %^g\12\12%?" :prepend t)
     ("t" "Errand" entry (file+headline "~/org/todo.org" "Errands")
      "** TODO %?")
     ("w" "Work task" entry (file "~/org/todo.org")
      "** TODO %?\12\12%a")
     ("p" "Portuguese vocab" entry (file "~/org/portuguese.org")
      "* Word                                                                :drill:\12  :PROPERTIES:\12  :DRILL_CARD_TYPE: twosided\12  :END:\12\12  Translate\12** Portugese\12  %^{Portugese}\12** English\12  %^{English}\12")))
 '(org-clock-display-default-range 'untilnow)
 '(org-confirm-babel-evaluate nil)
 '(org-default-notes-file "~/org/notes.org")
 '(org-duration-format 'h:mm)
 '(org-file-apps
   '((auto-mode . emacs) (directory . emacs) ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default) ("\\.pdf\\'" . emacs)))
 '(org-fontify-done-headline t)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-modules '(ol-info ol-eshell ol-notmuch))
 '(org-return-follows-link t)
 '(org-src-tab-acts-natively t)
 '(pdf-annot-activate-created-annotations t)
 '(pdf-view-display-size 'fit-page)
 '(pop-up-frame-alist '((title . "emacs-popup-frame")))
 '(powerline-default-separator 'bar)
 '(projectile-enable-caching t)
 '(prolog-compile-string
   '((eclipse "[%f].") (mercury "mmake ")
     (sicstus
      (eval
       (if (prolog-atleast-version '(3 . 7))
           "prolog:zap_file(%m,%b,compile,%l)."
         "prolog:zap_file(%m,%b,compile).")))
     (swi "make.") (t "compile(%f).")))
 '(prolog-left-indent-regexp "\\(;\\|\\*?->\\)")
 '(prolog-program-name
   '(((getenv "EPROLOG") (eval (getenv "EPROLOG"))) (eclipse "eclipse")
     (mercury nil) (sicstus "sicstus") (swi "swipl") (gnu "gprolog")
     (t "swipl") (logtalk "~/bin/swilgt")))
 '(prolog-program-switches '((sicstus ("-i")) (t nil)))
 '(prolog-system 'swi)
 '(prolog-use-standard-consult-compile-method-flag t)
 '(scroll-bar-mode nil)
 '(send-mail-function 'smtpmail-send-it)
 '(tab-bar-show 1)
 '(tab-bar-tab-hints t)
 '(tool-bar-mode nil)
 '(undo-tree-auto-save-history t)
 '(use-package-hook-name-suffix nil)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-face-semhl-enum ((t (:inherit font-lock-variable-name-face :foreground "royal blue"))))
 '(lsp-face-semhl-member ((t (:inherit font-lock-variable-name-face :foreground "dark blue"))))
 '(lsp-face-semhl-operator ((t (:inherit font-lock-function-name-face :foreground "chocolate"))))
 '(org-headline-done ((t (:strike-through t))))
 '(vterm-color-black ((t (:background "#000000" :foreground "#000000"))))
 '(vterm-color-cyan ((t (:background "#2ea6ee" :foreground "#2ea6ee"))))
 '(vterm-color-green ((t (:background "#338e33" :foreground "#338e33"))))
 '(vterm-color-white ((t (:background "#ffffff" :foreground "#ffffff")))))
