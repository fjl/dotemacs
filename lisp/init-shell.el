(require 'shell)

;;;###autoload
(defun fjl/shell-mode-hook ()
  (setq tab-width 8))

;;;###autoload
(add-hook 'shell-mode-hook 'fjl/shell-mode-hook)
