(require 'markdown-mode)
(require 'init-commands)

;;;###autoload
(defun fjl/markdown-mode-hook ()
  (setq truncate-lines nil)
  (setq word-wrap t)
  (setq indent-tabs-mode nil)
  (variable-pitch-mode 1))

;;;###autoload
(add-hook 'markdown-mode-hook 'fjl/markdown-mode-hook)
