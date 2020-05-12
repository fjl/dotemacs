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

(defun fjl/treemacs-hook ()
 (treemacs-git-mode 'deferred))

;;;###autoload
(advice-add 'treemacs :before 'fjl/treemacs-setup)

(provide 'init-treemacs)
