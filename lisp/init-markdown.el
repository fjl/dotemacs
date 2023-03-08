;; -*- lexical-binding: t -*-

(require 'markdown-mode)

;;;###autoload
(defun fjl/markdown-mode-hook ()
  (setq truncate-lines nil)
  (setq word-wrap t)
  (setq indent-tabs-mode nil))

;;;###autoload
(add-hook 'markdown-mode-hook 'fjl/markdown-mode-hook)
