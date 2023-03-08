;; -*- lexical-binding: t -*-

(require 'org)
(require 'org-agenda)

(setq org-agenda-custom-commands
      '(("D" "Desktop Agenda"
         ((todo "NOW"
                 ((org-agenda-prefix-format "%-10c:")
                  (org-deadline-warning-days 30)
                  (org-agenda-sorting-strategy '(tag-up priority-down))
                  (org-agenda-todo-keyword-format "")
                  (org-agenda-overriding-header "NOW")))
          (agenda "" ((org-agenda-ndays 1)
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-prefix-format "%?-12t%-11s"))))
         ((org-agenda-with-colors t)
          (org-agenda-remove-tags t)
          (org-agenda-compact-blocks nil)
          (org-agenda-block-separator ""))
         ("~/.desktop-agenda.txt"))))

(defun fjl/org-table-check ()
  "Insert a checkmark and realign the current table."
  (interactive)
  (insert-char ?✓)
  (org-table-align))

;;;###autoload
(defun fjl/org-mode-hook ()
  (local-set-key (kbd "C-c x") 'fjl/org-table-check)
  (local-set-key (kbd "C-c r") 'org-table-insert-row)

  ;; Improve non-English export.
  (add-to-list 'org-latex-packages-alist '("AUTO" "babel" t ("pdflatex")))
  (add-to-list 'org-latex-packages-alist '("AUTO" "polyglossia" t ("xelatex" "lualatex")))

  (require 'ob-go))

;;;###autoload
(add-hook 'org-mode-hook 'fjl/org-mode-hook)

(provide 'init-org)
