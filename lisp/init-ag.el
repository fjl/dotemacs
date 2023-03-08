;; -*- lexical-binding: t -*-

;; This configures ag -- also known as 'the silver searcher'.

(require 'ag)
(require 'wgrep-ag)

;; Enable wgrep in ag buffers.
(add-hook 'ag-mode-hook 'wgrep-ag-setup)

;;;###autoload
(defun project-ag (project string)
  "Searches for STRING in the current project.el project."
  (interactive (list (or (project-current)
                         (project-prompt-project-dir))
                     (ag/read-from-minibuffer "Search string")))
  (unless (stringp project)
    (setq project (project-root project)))
  (ag/search string project))

(provide 'init-ag)
