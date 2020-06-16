(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["light gray" "#f36c60" "medium sea green" "dark goldenrod" "dark cyan" "dark magenta" "blue" "#263238"])
 '(auth-sources (quote ("~/.authinfo.gpg" "~/.netrc")))
 '(calendar-week-start-day 1)
 '(clojure-indent-style :align-arguments)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(custom-safe-themes
   (quote
    ("98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" default)))
 '(dired-guess-shell-alist-user
   (quote
    (("\\.wav"
      (if
          (string-equal system-type "darwin")
          "afplay" "aplay")))))
 '(dired-listing-switches "-alh")
 '(display-line-numbers-type (quote relative))
 '(display-raw-bytes-as-hex t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(elfeed-goodies/powerline-default-separator (quote utf-8))
 '(elfeed-search-face-alist
   (quote
    ((unread elfeed-search-unread-title-face)
     (mustread elfeed-log-info-level-face)
     (comic elfeed-log-debug-level-face)
     (busy shadow))))
 '(erc-autojoin-channels-alist
   (quote
    (("localhost:6667" "#fb-family" "#lhl-teachers" "#fb-western-chums" "&bitlbee"))))
 '(erc-autojoin-timing (quote ident))
 '(erc-join-buffer (quote bury))
 '(erc-lurker-hide-list (quote ("JOIN" "PART" "QUIT" "MODE")))
 '(erc-modules
   (quote
    (hl-nicks autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notifications readonly ring stamp spelling track image)))
 '(erc-track-exclude-types (quote ("JOIN" "NICK" "PART" "QUIT" "MODE" "333" "353")))
 '(eshell-visual-commands
   (quote
    ("htop" "vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm" "psql")))
 '(eshell-visual-subcommands (quote (("git " "log" "lol" "diff" "show"))))
 '(evil-search-module (quote evil-search))
 '(evil-surround-pairs-alist
   (quote
    ((40 "( " . " )")
     (91 "[ " . " ]")
     (123 "{ " . " }")
     (41 "(" . ")")
     (93 "[" . "]")
     (125 "{" . "}")
     (35 "#{" . "}")
     (98 "(" . ")")
     (66 "{" . "}")
     (62 "<" . ">")
     (116 . evil-surround-read-tag)
     (60 . evil-surround-read-tag)
     (102 . evil-surround-function)
     (124 "「" . "」")
     (107 "【" . "】"))))
 '(exec-path-from-shell-arguments (quote ("-l")))
 '(exec-path-from-shell-shell-name "/usr/bin/fish")
 '(eyebrowse-mode-line-left-delimiter "❲")
 '(eyebrowse-mode-line-right-delimiter "❳")
 '(eyebrowse-mode-line-separator " ")
 '(eyebrowse-new-workspace t)
 '(flycheck-checkers
   (quote
    (ada-gnat asciidoctor asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint cuda-nvcc cwl d-dmd dockerfile-hadolint emacs-lisp emacs-lisp-checkdoc erlang-rebar3 erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert go-megacheck go-staticcheck groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jshint javascript-standard json-jsonlint json-python-json jsonnet less less-stylelint llvm-llc lua-luacheck lua markdown-markdownlint-cli markdown-mdl nix nix-linter opam perl perl-perlcritic php php-phpmd php-phpcs processing protobuf-protoc pug puppet-parser puppet-lint python-flake8 python-pylint python-pycompile python-mypy r-lintr racket rpm-rpmlint rst-sphinx rst ruby-rubocop ruby-reek ruby-rubylint ruby ruby-jruby rust-cargo rust rust-clippy scala scala-scalastyle scheme-chicken scss-lint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tcl-nagelfar tex-chktex tex-lacheck texinfo textlint typescript-tslint verilog-verilator vhdl-ghdl xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby)))
 '(flycheck-color-mode-line-face-to-color (quote cogent-line-buffer-name-face))
 '(flycheck-proselint-executable "~/.pyenv/shims/proselint")
 '(forge-bug-reference-hooks (quote (git-commit-setup-hook magit-mode-hook)))
 '(helm-imenu-fuzzy-match t)
 '(helm-rg-file-paths-in-matches-behavior (quote absolute))
 '(helm-semantic-fuzzy-match t)
 '(helm-use-undecorated-frame-option t)
 '(hl-sexp-background-color "#1c1f26")
 '(load-prefer-newer t)
 '(lsp-enable-text-document-color nil)
 '(lsp-ui-doc-border "royal blue")
 '(lsp-ui-doc-header nil)
 '(lsp-ui-doc-max-height 50)
 '(lsp-ui-doc-max-width 80)
 '(lsp-ui-doc-position (quote top))
 '(lsp-ui-doc-use-webkit nil)
 '(lsp-ui-peek-always-show nil)
 '(lsp-ui-peek-list-width 50)
 '(lsp-ui-peek-peek-height 10)
 '(lsp-ui-sideline-show-hover nil)
 '(magit-diff-refine-hunk t)
 '(menu-bar-mode t)
 '(mode-require-final-newline nil)
 '(moody-mode-line-height 28)
 '(mouse-drag-and-drop-region (quote modifier))
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "new" :query "tag:inbox and tag:unread")
     (:name "flagged" :query "tag:flagged"))))
 '(org-agenda-compact-blocks t)
 '(org-agenda-custom-commands
   (quote
    (("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil)
     ("N" "Notebook search" tags ""
      ((org-agenda-files
        (quote
         ("~/org/notebook"))))))))
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-show-future-repeats nil)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-babel-clojure-backend (quote cider))
 '(org-babel-load-languages (quote ((shell . t) (emacs-lisp . t) (dot . t))))
 '(org-bullets-bullet-list (quote ("●" "◉" "◎" "○")))
 '(org-capture-templates
   (quote
    (("n" "Note" plain
      (file cogent/org-note-file-name)
      "#+DATE: %t
* %^{title}    %^g

%?" :prepend t)
     ("t" "Errand" entry
      (file+headline "~/org/todo.org" "Errands")
      "** TODO %?")
     ("w" "Work task" entry
      (file "~/org/todo.org")
      "** TODO %?

   %a"))))
 '(org-confirm-babel-evaluate nil)
 '(org-fontify-done-headline t)
 '(org-hide-emphasis-markers t)
 '(org-modules (quote (ol-info ol-eshell ol-notmuch)))
 '(org-return-follows-link t)
 '(org-src-tab-acts-natively t)
 '(pdf-annot-activate-created-annotations t)
 '(pdf-view-display-size (quote fit-page))
 '(pop-up-frame-alist (quote ((title . "emacs-popup-frame"))))
 '(powerline-default-separator (quote bar))
 '(projectile-enable-caching t)
 '(prolog-compile-string
   (quote
    ((eclipse "[%f].")
     (mercury "mmake ")
     (sicstus
      (eval
       (if
           (prolog-atleast-version
            (quote
             (3 . 7)))
           "prolog:zap_file(%m,%b,compile,%l)." "prolog:zap_file(%m,%b,compile).")))
     (swi "make.")
     (t "compile(%f)."))))
 '(prolog-left-indent-regexp "\\(;\\|\\*?->\\)")
 '(prolog-program-name
   (quote
    (((getenv "EPROLOG")
      (eval
       (getenv "EPROLOG")))
     (eclipse "eclipse")
     (mercury nil)
     (sicstus "sicstus")
     (swi "~/.swivm/versions/v8.1.31/bin/swipl")
     (gnu "gprolog")
     (t "~/.swivm/versions/v8.1.31/bin/swipl")
     (logtalk "~/bin/swilgt"))))
 '(prolog-program-switches (quote ((sicstus ("-i")) (t nil))))
 '(prolog-system (quote swi))
 '(prolog-use-standard-consult-compile-method-flag t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(undo-tree-auto-save-history t)
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
 '(vc-annotate-very-old-color nil)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eyebrowse-mode-line-delimiters ((t (:foreground "#bd93f9"))))
 '(eyebrowse-mode-line-inactive ((t (:foreground "#6272a4"))))
 '(eyebrowse-mode-line-separator ((t (:foreground "#8292c4"))))
 '(helm-candidate-number ((t (:background "#44475a" :foreground "#f8f8f2"))))
 '(helm-prefarg ((t (:foreground "#50fa7b")))))
