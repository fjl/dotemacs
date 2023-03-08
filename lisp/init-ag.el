;; -*- lexical-binding: t -*-

;; This configures ag -- also known as 'the silver searcher'.

(require 'ag)
(require 'wgrep-ag)

;; Enable wgrep in ag buffers.
(add-hook 'ag-mode-hook 'wgrep-ag-setup)

;;;###autoload
(defun project-ag (project-dir string)
  "Searches for STRING in the current project.el project."
  (interactive
   (let ((d (project-root (project-current t))))
     (list d (ag/read-from-minibuffer (concat "ag in " d)))))
  (ag/search string project-dir))

(provide 'init-ag)
