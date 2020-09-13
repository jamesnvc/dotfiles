(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["light gray" "#f36c60" "medium sea green" "dark goldenrod" "dark cyan" "dark magenta" "blue" "#263238"])
 '(auth-sources '("~/.authinfo.gpg" "~/.netrc"))
 '(calendar-week-start-day 1)
 '(clojure-indent-style :align-arguments)
 '(confirm-kill-emacs 'yes-or-no-p)
 '(custom-safe-themes
   '("98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" default))
 '(dired-guess-shell-alist-user
   '(("\\.wav"
      (if
          (string-equal system-type "darwin")
          "afplay" "aplay"))))
 '(dired-listing-switches "-alh")
 '(display-line-numbers-type 'relative)
 '(display-raw-bytes-as-hex t)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(elfeed-search-face-alist
   '((unread elfeed-search-unread-title-face)
     (mustread elfeed-log-info-level-face)
     (comic elfeed-log-debug-level-face)
     (busy shadow)))
 '(erc-autojoin-channels-alist
   '(("localhost:6667" "#fb-family" "#lhl-teachers" "#fb-western-chums" "&bitlbee")))
 '(erc-autojoin-timing 'ident)
 '(erc-join-buffer 'bury)
 '(erc-lurker-hide-list '("JOIN" "PART" "QUIT" "MODE"))
 '(erc-modules
   '(hl-nicks autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notifications readonly ring stamp spelling track image))
 '(erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "333" "353"))
 '(eshell-visual-commands
   '("htop" "vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm" "psql"))
 '(eshell-visual-subcommands '(("git " "log" "lol" "diff" "show")))
 '(evil-buffer-regexps
   '(("^ \\*load\\*")
     ("^\\*org-goto\\*" . emacs)
     ("^\\*xref\\*" . emacs)))
 '(evil-search-module 'evil-search)
 '(evil-surround-pairs-alist
   '((40 "( " . " )")
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
     (107 "【" . "】")))
 '(exec-path-from-shell-arguments '("-l"))
 '(exec-path-from-shell-shell-name "/usr/bin/fish")
 '(eyebrowse-mode-line-left-delimiter "❲")
 '(eyebrowse-mode-line-right-delimiter "❳")
 '(eyebrowse-mode-line-separator " ")
 '(eyebrowse-new-workspace t)
 '(flycheck-checkers
   '(ada-gnat asciidoctor asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint cuda-nvcc cwl d-dmd dockerfile-hadolint emacs-lisp emacs-lisp-checkdoc erlang-rebar3 erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert go-megacheck go-staticcheck groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jshint javascript-standard json-jsonlint json-python-json jsonnet less less-stylelint llvm-llc lua-luacheck lua markdown-markdownlint-cli markdown-mdl nix nix-linter opam perl perl-perlcritic php php-phpmd php-phpcs processing protobuf-protoc pug puppet-parser puppet-lint python-flake8 python-pylint python-pycompile python-mypy r-lintr racket rpm-rpmlint rst-sphinx rst ruby-rubocop ruby-reek ruby-rubylint ruby ruby-jruby rust-cargo rust rust-clippy scala scala-scalastyle scheme-chicken scss-lint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tcl-nagelfar tex-chktex tex-lacheck texinfo textlint typescript-tslint verilog-verilator vhdl-ghdl xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby))
 '(flycheck-color-mode-line-face-to-color 'cogent-line-buffer-name-face)
 '(flycheck-proselint-executable "~/.pyenv/shims/proselint")
 '(forge-bug-reference-hooks '(git-commit-setup-hook magit-mode-hook))
 '(helm-imenu-fuzzy-match t)
 '(helm-rg-file-paths-in-matches-behavior 'absolute)
 '(helm-semantic-fuzzy-match t)
 '(helm-use-undecorated-frame-option t)
 '(hl-sexp-background-color "#1c1f26")
 '(load-prefer-newer t)
 '(lsp-enable-semantic-highlighting t)
 '(lsp-enable-text-document-color nil)
 '(lsp-semantic-tokens-apply-modifiers t)
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
 '(modus-operandi-theme-bold-constructs t)
 '(modus-operandi-theme-completions 'opinionated)
 '(modus-operandi-theme-org-blocks 'rainbow)
 '(modus-operandi-theme-slanted-constructs t)
 '(modus-operandi-theme-variable-pitch-headings t)
 '(moody-mode-line-height 28)
 '(mouse-drag-and-drop-region 'modifier)
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "new" :query "tag:inbox and tag:unread")
     (:name "flagged" :query "tag:flagged")))
 '(org-agenda-compact-blocks t)
 '(org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil)
     ("N" "Notebook search" tags ""
      ((org-agenda-files
        '("~/org/notebook"))))))
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-show-future-repeats nil)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-babel-clojure-backend 'cider)
 '(org-babel-load-languages '((shell . t) (emacs-lisp . t) (dot . t)))
 '(org-bullets-bullet-list '("●" "◉" "◎" "○"))
 '(org-capture-templates
   '(("n" "Note" plain
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

   %a")
     ("p" "Portuguese vocab" entry
      (file "~/org/portuguese.org")
      "* Word                                                                :drill:
  :PROPERTIES:
  :DRILL_CARD_TYPE: twosided
  :END:

  Translate
** Portugese
  %^{Portugese}
** English
  %^{English}
")))
 '(org-confirm-babel-evaluate nil)
 '(org-duration-format 'h:mm)
 '(org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . emacs)))
 '(org-fontify-done-headline t)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-modules '(ol-info ol-eshell org-notify ol-notmuch))
 '(org-return-follows-link t)
 '(org-src-tab-acts-natively t)
 '(pdf-annot-activate-created-annotations t)
 '(pdf-view-display-size 'fit-page)
 '(pop-up-frame-alist '((title . "emacs-popup-frame")))
 '(powerline-default-separator 'bar)
 '(projectile-enable-caching t)
 '(prolog-compile-string
   '((eclipse "[%f].")
     (mercury "mmake ")
     (sicstus
      (eval
       (if
           (prolog-atleast-version
            '(3 . 7))
           "prolog:zap_file(%m,%b,compile,%l)." "prolog:zap_file(%m,%b,compile).")))
     (swi "make.")
     (t "compile(%f).")))
 '(prolog-left-indent-regexp "\\(;\\|\\*?->\\)")
 '(prolog-program-name
   '(((getenv "EPROLOG")
      (eval
       (getenv "EPROLOG")))
     (eclipse "eclipse")
     (mercury nil)
     (sicstus "sicstus")
     (swi "~/.swivm/versions/v8.3.6/bin/swipl")
     (gnu "gprolog")
     (t "~/.swivm/versions/v8.3.6/bin/swipl")
     (logtalk "~/bin/swilgt")))
 '(prolog-program-switches '((sicstus ("-i")) (t nil)))
 '(prolog-system 'swi)
 '(prolog-use-standard-consult-compile-method-flag t)
 '(scroll-bar-mode nil)
 '(straight-recipe-overrides
   '((nil
      (company-emoji :host github :repo "dunn/company-emoji" :branch "trunk"))))
 '(tab-bar-mode t)
 '(tab-bar-show 1)
 '(tab-bar-tab-hints t)
 '(tool-bar-mode nil)
 '(undo-tree-auto-save-history t)
 '(use-package-hook-name-suffix nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#f36c60")
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
     (360 . "#8bc34a")))
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
 '(helm-prefarg ((t (:foreground "#50fa7b"))))
 '(helm-rg-base-rg-cmd-face ((t (:foreground "dark gray" :weight normal))))
 '(helm-rg-colon-separator-ripgrep-output-face ((t (:foreground "gray"))))
 '(helm-rg-extra-arg-face ((t (:foreground "goldenrod" :weight normal))))
 '(helm-rg-inactive-arg-face ((t (:foreground "dark gray" :weight normal))))
 '(lsp-face-semhl-enum ((t (:inherit font-lock-variable-name-face :foreground "royal blue"))))
 '(lsp-face-semhl-member ((t (:inherit font-lock-variable-name-face :foreground "dark blue"))))
 '(lsp-face-semhl-operator ((t (:inherit font-lock-function-name-face :foreground "chocolate"))))
 '(org-headline-done ((t (:strike-through t)))))
