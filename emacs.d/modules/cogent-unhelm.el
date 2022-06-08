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
  (setq completion-in-region-function #'consult-completion-in-region)
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

  (with-eval-after-load 'project
    (define-key project-prefix-map (kbd "i") #'consult-imenu-multi))

  ;; Enables previews inside the standard *Completions* buffer (what
  ;; `mct.el' uses).
  ;; (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

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
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "M-s M-i") #'consult-org-heading))

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
          orderless-regexp))
  (setq prot-orderless-alternative-styles
        '(orderless-literal
          orderless-prefixes
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

(defun cogent/switch-to-bookmark-horiz-split (bookmark)
  "Switch to file in a horizontal split"
  (interactive (list (bookmark-completing-read "Jump to bookmark"
                                               bookmark-current-bookmark)))
  (cogent--embark-act #'cogent--split-below #'bookmark-jump bookmark))
(defun cogent/switch-to-bookmark-vert-split (bookmark)
  "Switch to file in a vertical split"
  (interactive (list (bookmark-completing-read "Jump to bookmark"
                                               bookmark-current-bookmark)))
  (cogent--embark-act #'cogent--split-right #'bookmark-jump bookmark))

(defun cogent/switch-to-consult-grep-horiz-split (location)
  ;; don't need the extra wrapper thing that
  ;; `embark-consult-goto-grep' uses, because with the new setup, our
  ;; selected window is the minibuffer
  (split-window nil nil 'above)
  (consult--jump (consult--grep-position location))
  (pulse-momentary-highlight-one-line (point)))
(defun cogent/switch-to-consult-grep-vert-split (location)
  ;; don't need the extra wrapper thing that
  ;; `embark-consult-goto-grep' uses, because with the new setup, our
  ;; selected window is the minibuffer
  (split-window nil nil 'left)
  (consult--jump (consult--grep-position location))
  (pulse-momentary-highlight-one-line (point)))

(defun cogent/absolute-path-to-kill-ring (file)
  (kill-new (expand-file-name file)))

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
  (define-key embark-consult-search-map (kbd "C-s") #'cogent/switch-to-consult-grep-horiz-split)
  (define-key embark-consult-search-map (kbd "C-v") #'cogent/switch-to-consult-grep-vert-split)
  :general
  (:keymaps '(embark-buffer-map)
            "C-s" #'cogent/switch-to-buffer-horiz-split
            "C-v" #'cogent/switch-to-buffer-vert-split)
  (:keymaps '(embark-file-map)
            "P" #'cogent/absolute-path-to-kill-ring
            "C-s" #'cogent/switch-to-file-horiz-split
            "C-v" #'cogent/switch-to-file-vert-split)
  (:keymaps '(embark-bookmark-map)
            "C-s" #'cogent/switch-to-bookmark-horiz-split
            "C-v" #'cogent/switch-to-bookmark-vert-split))

(use-package embark-consult
  :after (embark consult)
  :config
  ;; after exporting ripgrep results to buffer, call
  ;; `next-error-select-buffer' to make that be the "error" buffer, then
  ;; you can use M-g M-n or C-x ` to go between "errors"
  (advice-add 'embark-consult-export-grep :after
              (lambda (&rest args) (next-error-select-buffer (current-buffer))))
  (define-key embark-consult-export-grep-map (kbd "Q") (lambda () (interactive) (quit-window t))))

(general-def
  "M-x" #'execute-extended-command
  "<menu>" #'execute-extended-command
  "C-x C-f" #'find-file
  "M-y" #'consult-yank-from-kill-ring)

(use-package prot-common
  :straight (:type built-in))

(use-package minibuffer
  :straight (:type built-in)
  :config
  (setq completion-styles
        '(substring initials flex partial-completion orderless basic))
  (setq completion-category-overrides
        '((file (styles . (partial-completion orderless)))))
  (setq completion-cycle-threshold 2)
  (setq completion-flex-nospace nil)
  (setq completion-pcm-complete-word-inserts-delimiters nil)
  (setq completion-pcm-word-delimiters "-_./:| ")
  (setq completion-show-help nil)
  (setq completion-auto-help t)
  (setq completion-ignore-case t)
  (setq completions-max-height 20)
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
  (setopt use-short-answers t)
  (setq resize-mini-windows t)
  (setq minibuffer-eldef-shorten-default t)

  (setq echo-keystrokes 0.25)           ; from the C source code

  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)
  (setq minibuffer-completion-auto-choose nil)

  ;; from `prot-common.el'
  (add-hook 'completion-list-mode-hook #'prot-common-truncate-lines-silently)

  (defun cogent/minibuffer-next-completion-group (&optional count)
    (interactive "p")
    (with-minibuffer-completions-window
      (when completions-highlight-face
        (setq-local cursor-face-highlight-nonselected-window t))
      (dotimes (_ (or count 1))
        (when-let (group (save-excursion
                           (text-property-search-forward 'face
                                                         'completions-group-separator
                                                         t nil)))
          (let ((pos (prop-match-end group)))
            (unless (eq pos (point-max))
              (goto-char pos)
              (next-completion 1)))))))

  (defun cogent/minibuffer-previous-completion-group (&optional count)
    (interactive "p")
    (with-minibuffer-completions-window
      (when completions-highlight-face
        (setq-local cursor-face-highlight-nonselected-window t))
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
          (next-line 1)))))

  (defun cogent/minibuffer-completion-beginning ()
    (interactive)
    (with-minibuffer-completions-window
      (goto-char (point-min))
      (next-completion 1)))

  (defun cogent/minibuffer-completion-end ()
    (interactive)
    (with-minibuffer-completions-window
      (goto-char (point-max))
      (previous-completion 1)))

  (general-define-key :states '(insert)
                      "C-\\" #'completion-at-point)


  (define-key minibuffer-local-completion-map [remap next-line] #'minibuffer-next-completion)
  (define-key minibuffer-local-completion-map [remap previous-line] #'minibuffer-previous-completion)
  (define-key minibuffer-local-completion-map (kbd "TAB") (lambda () (interactive) (minibuffer-choose-completion t)))
  (define-key minibuffer-local-completion-map (kbd "M-<right>") #'cogent/minibuffer-next-completion-group)
  (define-key minibuffer-local-completion-map (kbd "M-<left>") #'cogent/minibuffer-previous-completion-group)
  (define-key minibuffer-local-completion-map (kbd "M-<") #'cogent/minibuffer-completion-beginning)
  (define-key minibuffer-local-completion-map (kbd "M->") #'cogent/minibuffer-completion-end)
  ;; refresh completion candidates
  (define-key minibuffer-local-completion-map (kbd "C-l") #'minibuffer-completion-help))


(defmacro comment (&rest body)
  `(progn))

;; XXX: temporary hack to overwrite the function in crm.el until I re-build with my patch applied
(comment
 (defun completing-read-multiple
     (prompt table &optional predicate require-match initial-input
             hist def inherit-input-method)
   "Read multiple strings in the minibuffer, with completion.
The arguments are the same as those of `completing-read'.
\\<crm-local-completion-map>
Input multiple strings by separating each one with a string that
matches the regexp `crm-separator'.  For example, if the separator
regexp is \",\", entering \"alice,bob,eve\" specifies the strings
\"alice\", \"bob\", and \"eve\".

We refer to contiguous strings of non-separator-characters as
\"elements\".  In this example there are three elements.

Completion is available on a per-element basis.  For example, if the
contents of the minibuffer are \"alice,bob,eve\" and point is between
\"l\" and \"i\", pressing \\[minibuffer-complete] operates on the element \"alice\".

This function returns a list of the strings that were read,
with empty strings removed."
   (let* ((map (if require-match
                   crm-local-must-match-map
                 crm-local-completion-map))
          input)
     (minibuffer-with-setup-hook
         (lambda ()
           (add-hook 'choose-completion-string-functions
                     'crm--choose-completion-string nil 'local)
           (setq-local minibuffer-completion-table #'crm--collection-fn)
           (setq-local minibuffer-completion-predicate predicate)
           (setq-local completion-list-insert-choice-function
                       (lambda (start end choice)
                         (if (and (stringp start) (stringp end))
                             (let* ((beg (save-excursion
                                           (goto-char (minibuffer-prompt-end))
                                           (or (search-forward start nil t)
                                               (search-forward-regexp crm-separator nil t)
                                               (minibuffer-prompt-end))))
                                    (end (save-excursion
                                           (goto-char (point-max))
                                           (or (search-backward end nil t)
                                               (progn
                                                 (goto-char beg)
                                                 (search-forward-regexp crm-separator nil t))
                                               (point-max)))))
                               (completion--replace beg end choice))
                           (completion--replace start end choice))))
           ;; see completing_read in src/minibuf.c
           (setq-local minibuffer-completion-confirm
                       (unless (eq require-match t) require-match))
           (setq-local crm-completion-table table))
       (setq input (read-from-minibuffer
                    prompt initial-input map
                    nil hist def inherit-input-method)))
     ;; If the user enters empty input, `read-from-minibuffer'
     ;; returns the empty string, not DEF.
     (when (and def (string-equal input ""))
       (setq input (if (consp def) (car def) def)))
     ;; Remove empty strings in the list of read strings.
     (split-string input crm-separator t)))
 )

(provide 'cogent-unhelm)
