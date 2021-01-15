;;; -*- lexical-binding: t -*-
;;; Generate splitter functions for opening helm results in vertical &
;;; horizontal splits

(cl-macrolet
    ((make-splitter-fn (name open-fn split-fn)
                       `(defun ,name (_candidate)
                          ;; Display buffers in new windows
                          (dolist (cand (helm-marked-candidates))
                            (select-window (,split-fn))
                            (,open-fn cand))
                          ;; Adjust size of windows
                          (balance-windows)))
     (generate-helm-splitter-funcs
      (op-type open-fn)
      (let* ((prefix (concat "helm-" op-type "-switch-"))
             (vert-split (intern (concat prefix "vert-window")))
             (horiz-split (intern (concat prefix "horiz-window"))))
        `(progn
           (make-splitter-fn ,vert-split ,open-fn split-window-right)

           (make-splitter-fn ,horiz-split ,open-fn split-window-below)

           (defun ,(intern (concat "helm-" op-type "-switch-vert-window-command"))
               ()
             (interactive)
             (with-helm-alive-p
               (helm-exit-and-execute-action (quote ,vert-split))))

           (defun ,(intern (concat "helm-" op-type "-switch-horiz-window-command"))
               ()
             (interactive)
             (with-helm-alive-p
               (helm-exit-and-execute-action (quote ,horiz-split))))))))

  (generate-helm-splitter-funcs "buffer" switch-to-buffer)
  (generate-helm-splitter-funcs "file" find-file)
  (generate-helm-splitter-funcs "bookmark" helm-bookmark-jump))

(provide 'cogent-helm-splits)
