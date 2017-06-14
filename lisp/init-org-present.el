(require 'org-present)

;;;###autoload
(defun fjl/org-present-mode-hook ()
  (setq word-wrap t)
  (setq truncate-lines nil)
  (org-present-big)
  (org-display-inline-images)
  (org-present-hide-cursor)
  (org-present-read-only))

;;;###autoload
(defun fjl/org-present-mode-quit-hook ()
  (org-present-small)
  (org-remove-inline-images)
  (org-present-show-cursor)
  (org-present-read-write))

;;;###autoload
(add-hook 'org-present-mode-hook 'fjl/org-present-mode-hook)
(add-hook 'org-present-mode-quit-hook 'fjl/org-present-mode-quit-hook)

(provide 'init-org-present)
