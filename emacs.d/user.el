;; -*- lexical-binding: t -*-

(set-frame-font "PragmataPro Mono 7" nil t)

(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

(evil-leader/set-key
      "t" 'helm-find-files
      "o" 'helm-buffers-list
      "w" 'save-buffer
      "<SPC>" 'evil-ex
      "m" 'helm-M-x)

(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)

;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(defun cogent/paredit-vim-bindings ()
  (evil-define-key 'normal paredit-mode-map "W" 'paredit-forward)
  (evil-define-key 'normal paredit-mode-map "B" 'paredit-backward)
  (evil-define-key 'normal paredit-mode-map "\\@" 'paredit-splice-sexp)
  (evil-define-key 'normal paredit-mode-map "\\o" 'paredit-raise-sexp)
  (evil-define-key 'normal paredit-mode-map ">)" 'paredit-forward-slurp-sexp)
  (evil-define-key 'normal paredit-mode-map "<)" 'paredit-forward-barf-sexp)
  (evil-define-key 'normal paredit-mode-map "<(" 'paredit-backward-slurp-sexp)
  (evil-define-key 'normal paredit-mode-map ">(" 'paredit-backward-barf-sexp))

(add-hook 'paredit-mode-hook 'cogent/paredit-vim-bindings)
