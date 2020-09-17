(require 'magit)

(defun magit-visit-pull-request-url ()
  "Visit the current branch's PR on Github.
The branch must be pushed to github and have a remote."
  (interactive)
  (let* ((branch      (magit-get-current-branch))
         (branch-info (fjl/get-github-info branch (magit-get-push-remote branch)))
         (base        (fjl/get-branch-upstream branch))
         (base-info   (fjl/get-github-info (cdr base) (car base))))
    (browse-url
     (format "https://github.com/%s/%s/compare/%s...%s:%s"
             (car base-info) (cdr base-info) (cdr base) (car branch-info) branch))))

(defun magit-yank-github-url ()
  (interactive)
  (let* ((branch (magit-get-current-branch))
         (info   (fjl/get-github-info branch (magit-get-push-remote branch)))
         (url    (format "https://github.com/%s/%s/tree/%s"
                         (car info) (cdr info) branch)))
    (kill-new url)
    (message url)))

(defun fjl/get-branch-upstream (&optional branch)
  (let ((name (magit-split-branch-name (magit-get-upstream-branch branch))))
    (if (string= (car name) ".")
        (magit-split-branch-name (magit-get-push-branch (cdr name)))
      name)))

(defun fjl/get-github-info (branch remote-name)
  "Returns a cons containing the github username in the car and
the remote repository name in the cdr."
  (let ((remote (magit-get "remote" remote-name "url")))
    (when (null remote)
      (error "current branch has no remote"))
    (save-match-data
      (string-match "\\`.+github\\.com[:/]\\([^\\]+\\)/\\([^\\]+?\\)\\(\\.git\\)?\\'" remote)
      (cons (match-string 1 remote)
            (match-string 2 remote)))))

;;;###autoload
(defun fjl/magit-mode-hook ()
  (when (string= system-type "darwin")
    (setq magit-git-executable "/usr/local/bin/git"))
  (define-key magit-mode-map (kbd "v") 'magit-visit-pull-request-url))

;;;###autoload
(add-hook 'magit-mode-hook 'fjl/magit-mode-hook)
