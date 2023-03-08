(require 'magit)

(defun magit-visit-pull-request-url ()
  "Visit the current branch's PR on Github.
The branch must be pushed to github and have a remote."
  (interactive)
  (let* ((branch (magit-get-current-branch))
         (remote (fjl/get-branch-remote branch)))
    (unless remote
      (error "Current branch has no remote."))
    (let ((branch-info (fjl/get-github-info branch remote)))
      (unless branch-info
        (error "Current branch remote is not GitHub."))
      (let* ((base (fjl/get-branch-upstream branch))
             (base-info (fjl/get-github-info (cdr base) (car base))))
        (unless base-info
          (error "Upstream is not on GitHub."))
        (browse-url
         (format "https://github.com/%s/%s/compare/%s...%s:%s"
                 (car base-info) (cdr base-info) (cdr base) (car branch-info) branch))))))

(defun magit-yank-github-url ()
  (interactive)
  (let* ((branch (magit-get-current-branch))
         (info (fjl/get-github-info branch (fjl/get-branch-remote branch))))
    (unless info
      (error "Current branch has no GitHub remote."))
    (let ((url (format "https://github.com/%s/%s/tree/%s" (car info) (cdr info) branch)))
      (kill-new url)
      (message url))))

(defun fjl/get-branch-remote (branch)
  (or (magit-get-push-remote branch)
      (magit-get-upstream-remote branch)))

(defun fjl/get-branch-upstream (&optional branch)
  (let ((name (magit-split-branch-name (magit-get-upstream-branch branch))))
    (if (string= (car name) ".")
        (magit-split-branch-name (magit-get-push-branch (cdr name)))
      name)))

(defun fjl/get-github-info (branch remote-name)
  "Returns a cons containing the github username in the car and
the remote repository name in the cdr."
  (let ((remote (magit-get "remote" remote-name "url")))
    (when remote
      (save-match-data
        (when (string-match "\\`.+github\\.com[:/]\\([^\\]+\\)/\\([^\\]+?\\)\\(\\.git\\)?\\'" remote)
          (cons (match-string 1 remote)
                (match-string 2 remote)))))))

(defun fjl/mac-git-path ()
  (if (string-prefix-p "aarch64-" system-configuration)
      "/opt/homebrew/bin/git"
      "/usr/local/bin/git"))

;;;###autoload
(defun fjl/magit-mode-hook ()
  ;; On macOS, the default git in path is an XCode-enabled wrapper, making it a lot slower
  ;; to invoke. Just calling the regular git exectuable makes magit a bit faster.
  (when (and (not (file-remote-p default-directory)) (eq system-type 'darwin))
    (let ((exe (fjl/mac-git-path)))
      (when (file-exists-p exe)
        (setq-local magit-git-executable exe))))
  ;; Bindings
  (define-key magit-mode-map (kbd "v") 'magit-visit-pull-request-url)
  ;; Configure revision buffer.
  (setq magit-revision-insert-related-refs nil)
  ;; Configure status buffer.
  (setq magit-section-initial-visibility-alist '((untracked . hide) (stashes . hide) (unstaged . show) (unpushed . show)))
  (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header)
  ;; Configure refs buffer.
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  (remove-hook 'magit-refs-sections-hook 'magit-insert-remote-branches))

;;;###autoload
(add-hook 'magit-mode-hook 'fjl/magit-mode-hook)
