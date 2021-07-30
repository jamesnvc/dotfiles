;;; -*- lexical-binding: t -*-

(require 'cogent-package)

(use-package marginalia
  :straight (marginalia
             :type git
             :host github
             :repo "minad/marginalia"
             :branch "main")
  :init
  (marginalia-mode 1)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light
                           nil)))

(use-package wgrep)

(use-package consult
  :straight (consult
             :type git
             :host github
             :repo "minad/consult"
             :branch "main")
  :config
  (setq consult-line-numbers-widen t)
  ;; ;; FIXME 2021-04-10: This does not work with `prot-minibuffer.el'.
  ;; (setq completion-in-region-function #'consult-completion-in-region)
  (setq consult-async-min-input 3)
  (setq consult-async-input-debounce 0.5)
  (setq consult-async-input-throttle 0.8)
  (setq consult-narrow-key ">")
  (setq consult-imenu-config
        '((emacs-lisp-mode :toplevel "Functions"
                           :types ((?f "Functions" font-lock-function-name-face)
                                   (?m "Macros"    font-lock-keyword-face)
                                   (?p "Packages"  font-lock-constant-face)
                                   (?t "Types"     font-lock-type-face)
                                   (?v "Variables" font-lock-variable-name-face)))))
  (setq register-preview-delay 0.8
        register-preview-function #'consult-register-format)
  (setq consult-find-command "find . -iname *ARG* OPTS")
  (setq consult-preview-key 'any)

  ;; Enables previews inside the standard *Completions* buffer (what
  ;; `prot-minibuffer.el' uses).
  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

  (let ((map global-map))
    (define-key map (kbd "C-x M-:") #'consult-complex-command)
    (define-key map (kbd "C-x M-m") #'consult-minor-mode-menu)
    (define-key map (kbd "C-x M-k") #'consult-kmacro)
    (define-key map (kbd "M-g M-g") #'consult-goto-line)
    (define-key map (kbd "M-K") #'consult-keep-lines) ; M-S-k is similar to M-S-5 (M-%)
    (define-key map (kbd "M-F") #'consult-focus-lines) ; same principle
    (define-key map (kbd "M-s M-b") #'consult-buffer)
    (define-key map (kbd "M-s M-f") #'consult-find)
    (define-key map (kbd "M-s M-g") #'consult-grep)
    (define-key map (kbd "M-s M-m") #'consult-mark)
    (define-key map (kbd "C-x r r") #'consult-register)) ; Use the register's prefix
  (define-key consult-narrow-map (kbd "?") #'consult-narrow-help)

  :general
  (cogent/leader-def
    :states '(normal visual)
    "b" #'consult-buffer
    "l" #'consult-line
    "s" #'consult-ripgrep))

(use-package prot-consult
  :straight (:type built-in)
  :config
  (setq consult-project-root-function #'prot-consult-project-root)
  (setq prot-consult-command-centre-list
        '(consult-line
          prot-consult-line
          consult-mark))
  (setq prot-consult-command-top-list
        '(consult-outline
          consult-imenu
          prot-consult-outline
          prot-consult-imenu))
  (prot-consult-set-up-hooks-mode 1)
  (let ((map global-map))
    (define-key map (kbd "M-s M-i") #'prot-consult-imenu)
    (define-key map (kbd "M-s M-s") #'prot-consult-outline)
    (define-key map (kbd "M-s M-y") #'prot-consult-yank)
    (define-key map (kbd "M-s M-l") #'prot-consult-line)))

(use-package prot-orderless
  :straight (:type built-in)
  :config
  (setq prot-orderless-default-styles
        '(orderless-prefixes
          orderless-strict-leading-initialism
          orderless-regexp))
  (setq prot-orderless-alternative-styles
        '(orderless-literal
          orderless-prefixes
          orderless-strict-leading-initialism
          orderless-regexp)))

(use-package orderless
  :config
  (setq orderless-component-separator " +")
  (setq orderless-matching-styles prot-orderless-default-styles)
  (setq orderless-style-dispatchers
        '(prot-orderless-literal-dispatcher
          prot-orderless-initialism-dispatcher
          prot-orderless-flex-dispatcher))
  :bind (:map minibuffer-local-completion-map
              ("SPC" . nil)))

(defun cogent--split-below (open-fn target)
  (select-window (split-window-below))
  (funcall open-fn target))
(defun cogent--split-right (open-fn target)
  (select-window (split-window-right))
  (funcall open-fn target))

(defun cogent--embark-act (fn &rest args)
  "Helper function to handle embark act events that can run from completion"
  (when (and (eq (selected-window) (active-minibuffer-window))
             (not (minibufferp)))
    (apply #'embark--quit-and-run fn args))
  (apply fn args))

(defun cogent/switch-to-buffer-horiz-split (buf)
  "Switch to buffer in a horizontal split"
  (interactive "BBuffer: ")
  (cogent--embark-act #'cogent--split-below #'switch-to-buffer buf))
(defun cogent/switch-to-buffer-vert-split (buf)
  "Switch to buffer in a vertical split"
  (interactive "BBuffer: ")
  (cogent--embark-act #'cogent--split-right #'switch-to-buffer buf))

(defun cogent/switch-to-file-horiz-split (file)
  "Switch to file in a horizontal split"
  (interactive "FFile: ")
  (cogent--embark-act #'cogent--split-below #'find-file file))
(defun cogent/switch-to-file-vert-split (file)
  "Switch to file in a vertical split"
  (interactive "FFile: ")
  (cogent--embark-act #'cogent--split-right #'find-file file))

(use-package embark
  :bind
  ("C-," . #'embark-act)
  :config
  (evil-set-initial-state 'embark-collect-mode 'emacs)
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)
  (setq embark-collect-initial-view-alist
        '((file . list)
          (buffer . list)
          (symbol . list)
          (line . list)
          (xref-location . list)
          (kill-ring . zebra)
          (t . list)))
  (setq embark-quit-after-action t)
  :general
  (:keymaps '(embark-buffer-map)
            "C-s" #'cogent/switch-to-buffer-horiz-split
            "C-v" #'cogent/switch-to-buffer-vert-split)
  (:keymaps '(embark-file-map)
            "C-s" #'cogent/switch-to-file-horiz-split
            "C-v" #'cogent/switch-to-file-vert-split))

(use-package embark-consult
  :after (embark consult))

(general-def
  "M-x" #'execute-extended-command
  "<menu>" #'execute-extended-command
  "C-x C-f" #'find-file
  "M-y" #'consult-yank)

(use-package prot-common
  :straight (:type built-in))

(use-package prot-minibuffer
  :straight (:type built-in)
  :custom
  ((prot-minibuffer-remove-shadowed-file-names t)
   (prot-minibuffer-mini-cursors t)))

(defun cogent/completion-select-candidate ()
  (interactive)
  (when (and (derived-mode-p 'completion-list-mode)
             (active-minibuffer-window))
    (let ((choice (prot-minibuffer--completion-at-point)))
      (select-window (active-minibuffer-window))
      (delete-minibuffer-contents)
      (insert choice))))

(defun cogent/completion-next-group (&optional count)
  "Move to the next completion group"
  (interactive "p")
  (dotimes (_ (or count 1))
    (when-let (group (save-excursion
                       (text-property-search-forward 'face
                                                     'completions-group-separator
                                                     t nil)))
      (let ((pos (prop-match-end group)))
        (unless (eq pos (point-max))
          (goto-char pos)
          (next-completion 1))))))

(defun cogent/completion-prev-group (&optional count)
  "Move to the previous completion group"
  (interactive "p")
  (dotimes (_ (or count 1))
    ;; skip back, so if we're at the top of a group, we go to the previous one...
    (next-line -1)
    (if-let (group (save-excursion
                     (text-property-search-backward 'face
                                                    'completions-group-separator
                                                    t nil)))
        (let ((pos (prop-match-beginning group)))
          (unless (eq pos (point-min))
            (goto-char pos)
            (next-completion 1)))
      ;; ...and if there was a match, go back down, so the point doesn't
      ;; end in the group separator
      (next-line 1))))

(use-package minibuffer
  :straight (:type built-in)
  :config
  (setq completion-styles
        '(substring initials flex partial-completion orderless))
  (setq completion-category-overrides
        '((file (styles . (partial-completion orderless)))))
  (setq completion-cycle-threshold 2)
  (setq completion-flex-nospace nil)
  (setq completion-pcm-complete-word-inserts-delimiters nil)
  (setq completion-pcm-word-delimiters "-_./:| ")
  (setq completion-show-help nil)
  (setq completion-auto-help t)
  (setq completion-ignore-case t)
  (setq-default case-fold-search t)   ; For general regexp

  ;; The following two are updated in Emacs 28.  They concern the
  ;; *Completions* buffer.
  (setq completions-format 'one-column)
  (setq completions-detailed t)

  ;; Grouping of completions for Emacs 28
  (setq completions-group t)
  (setq completions-group-sort nil)
  (setq completions-group-format
        (concat
         (propertize "    " 'face 'completions-group-separator)
         (propertize " %s " 'face 'completions-group-title)
         (propertize " " 'face 'completions-group-separator
                     'display '(space :align-to right))))

  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)

  (setq enable-recursive-minibuffers t)
  (setq read-answer-short t) ; also check `use-short-answers' for Emacs28
  (setq resize-mini-windows t)
  (setq minibuffer-eldef-shorten-default t)

  (setq echo-keystrokes 0.25)           ; from the C source code

  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)

  (add-hook 'completion-list-mode-hook #'prot-common-truncate-lines-silently) ; from `prot-common.el'

  (define-key global-map (kbd "s-v") #'prot-minibuffer-focus-mini-or-completions)
  (let ((map completion-list-mode-map))
    (define-key map (kbd "h") #'prot-simple-describe-symbol) ; from `prot-simple.el'
    (define-key map (kbd "M-g") #'prot-minibuffer-choose-completion-number)
    (define-key map (kbd "M-v") #'prot-minibuffer-focus-minibuffer)
    (define-key map (kbd "C-g") #'prot-minibuffer-keyboard-quit-dwim)
    (define-key map (kbd "C-n") #'prot-minibuffer-next-completion-or-mini)
    (define-key map (kbd "<down>") #'prot-minibuffer-next-completion-or-mini)
    (define-key map (kbd "C-p") #'prot-minibuffer-previous-completion-or-mini)
    (define-key map (kbd "<up>") #'prot-minibuffer-previous-completion-or-mini)
    (define-key map (kbd "<return>") #'prot-minibuffer-choose-completion-exit)
    (define-key map (kbd "<right>") #'cogent/completion-next-group)
    (define-key map (kbd "<left>") #'cogent/completion-prev-group)
    (define-key map (kbd "<M-return>") #'prot-minibuffer-choose-completion-dwim)
    (define-key map (kbd "<tab>") #'cogent/completion-select-candidate)
    (define-key map (kbd "M-<") #'prot-minibuffer-beginning-of-buffer)
    (define-key map (kbd "<escape>") #'prot-minibuffer-keyboard-quit-dwim)
    ;; Those are generic actions for the "*Completions*" buffer, though
    ;; I normally use `embark'.
    (define-key map (kbd "w") #'prot-minibuffer-completions-kill-symbol-at-point)
    (define-key map (kbd "i") #'prot-minibuffer-completions-insert-symbol-at-point)
    (define-key map (kbd "j") #'prot-minibuffer-completions-insert-symbol-at-point-exit))
  (let ((map minibuffer-local-completion-map))
    (define-key map (kbd "M-g") #'prot-minibuffer-choose-completion-number)
    (define-key map (kbd "C-n") #'prot-minibuffer-switch-to-completions-top)
    (define-key map (kbd "<down>") #'prot-minibuffer-switch-to-completions-top)
    (define-key map (kbd "C-p") #'prot-minibuffer-switch-to-completions-bottom)
    (define-key map (kbd "<up>") #'prot-minibuffer-switch-to-completions-bottom)
    (define-key map (kbd "C-l") #'prot-minibuffer-toggle-completions)) ; "list" mnemonic
  (let ((map minibuffer-local-filename-completion-map))
    (define-key map (kbd "<M-backspace>") #'prot-minibuffer-backward-updir))
  (add-hook 'minibuffer-setup-hook #'prot-minibuffer-mini-cursor)
  (add-hook 'completion-list-mode-hook #'prot-minibuffer-completions-cursor)
  (add-hook 'completion-list-mode-hook #'prot-minibuffer-hl-line)
  (add-hook 'completion-list-mode-hook #'prot-minibuffer-display-line-numbers))

(provide 'cogent-unhelm)
