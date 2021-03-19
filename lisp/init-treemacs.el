(require 'treemacs)

;;;###autoload
(defun fjl/treemacs-setup ()
  (setq treemacs-collapse-dirs 3
        treemacs-display-in-side-window t
        treemacs-filewatch-mode t
        treemacs-follow-mode t
        treemacs-fringe-indicator-mode t
        treemacs-no-png-images t
        treemacs-position 'right
        treemacs-git-mode 'deferred)
  (setq treemacs-user-mode-line-format
        '("" (:eval (fjl/mode-line-align-right nil mode-line-misc-info)))))

;;;###autoload
(defun fjl/treemacs-hook ()
  ;; Copy ace-window mode line indicator.
  (let ((fmt (default-value 'mode-line-format)))
    (when (and (consp (car fmt))
               (eq (caar fmt) 'ace-window-display-mode))
        (add-to-list 'mode-line-format (car fmt)))))

;;;###autoload
(progn
  (advice-add 'treemacs :before 'fjl/treemacs-setup)
  (add-hook 'treemacs-mode-hook 'fjl/treemacs-hook))

(provide 'init-treemacs)
