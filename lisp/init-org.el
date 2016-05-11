(require 'org)

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

(provide 'init-org)
