(require 'magit)

(defvar *pr-base-branch* "develop")

(defun magit-visit-pull-request-url ()
  "Visit the current branch's PR on Github.
The branch must be pushed to github and have a remote."
  (interactive)
  (let* ((branch      (magit-get-current-branch))
         (base-info   (fjl/get-github-info *pr-base-branch* #'magit-get-remote))
         (branch-info (fjl/get-github-info branch #'magit-get-push-remote)))
    (browse-url
     (format "https://github.com/%s/%s/compare/%s...%s:%s"
             (car base-info) (cdr base-info) *pr-base-branch* (car branch-info) branch))))


(defun magit-yank-github-url ()
  (interactive)
  (let* ((branch (magit-get-current-branch))
         (info   (fjl/get-github-info branch #'magit-get-push-remote))
         (url    (format "https://github.com/%s/%s/tree/%s"
                         (car info) (cdr info) branch)))
    (kill-new url)
    (message url)))

(defun fjl/get-github-info (branch get-remote)
  "Returns a cons containing the github username in the car and
the remote repository name in the cdr."
  (let ((remote (magit-get "remote" (funcall get-remote branch) "url")))
    (when (null remote)
      (error "current branch has no remote"))
    (save-match-data
      (string-match "\\`.+github\\.com[:/]\\([^\\]+\\)/\\([^\\]+?\\)\\(\\.git\\)?\\'" remote)
      (cons (match-string 1 remote)
            (match-string 2 remote)))))

;;;###autoload
(defun fjl/magit-mode-hook ()
  (define-key magit-mode-map (kbd "v") 'magit-visit-pull-request-url))

;;;###autoload
(add-hook 'magit-mode-hook 'fjl/magit-mode-hook)
