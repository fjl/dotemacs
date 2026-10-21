;; -*- lexical-binding: t -*-

(require 'magit)
(declare-function fjl/mac-git-path "init-ui")

(defun magit-visit-pull-request-url ()
  "Visit the current branch's PR on Github.
The branch must be pushed to github and have a remote."
  (interactive)
  (let* ((branch (magit-get-current-branch))
         (remote (fjl/get-branch-remote branch)))
    (unless remote
      (error "Current branch has no remote."))
    (let ((branch-info (fjl/get-github-info remote)))
      (unless branch-info
        (error "Current branch remote is not GitHub."))
      (let* ((base (fjl/get-branch-upstream branch))
             (base-info (fjl/get-github-info (car base))))
        (unless base-info
          (error "Upstream is not on GitHub."))
        (browse-url
         (format "https://github.com/%s/%s/compare/%s...%s:%s"
                 (car base-info) (cdr base-info) (cdr base) (car branch-info) branch))))))

(defun magit-yank-github-url ()
  (interactive)
  (let* ((branch (magit-get-current-branch))
         (info (fjl/get-github-info (fjl/get-branch-remote branch))))
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

(defun fjl/get-github-info (remote-name)
  "Returns a cons containing the github username in the car and
the remote repository name in the cdr."
  (let ((remote (magit-get "remote" remote-name "url")))
    (when remote
      (save-match-data
        (when (string-match "\\`.+github\\.com[:/]\\([^\\]+\\)/\\([^\\]+?\\)\\(\\.git\\)?\\'" remote)
          (cons (match-string 1 remote)
                (match-string 2 remote)))))))

;;; Magit over TRAMP.
;;
;; Every remote command costs a network round trip, and a magit refresh over TRAMP
;; sent about 75 of them for ~20 git invocations, because TRAMP throws away its file
;; cache after each one. Measured with a 300ms RTT, keeping the cache for read-only
;; git commands took a status refresh from 74 to 45 remote commands.

(defun fjl/magit-git-read-only-p (args)
  "Non-nil if the git invocation ARGS cannot change the working tree or refs."
  (let* ((args (flatten-tree args))
         (cmd (seq-find (lambda (a) (not (string-prefix-p "-" a))) args))
         (rest (cdr (member cmd args))))
    (pcase cmd
      ((or "rev-parse" "rev-list" "diff" "diff-tree" "diff-index" "diff-files" "log" "show"
           "status" "for-each-ref" "show-ref" "ls-files" "ls-tree" "cat-file" "merge-base"
           "describe" "name-rev" "verify-tag" "version" "blame" "check-ignore"
           "check-ref-format" "count-objects" "shortlog" "reflog" "grep" "cherry"
           "ls-remote" "var" "when-merged")
       t)
      ;; symbolic-ref writes when given a second ref.
      ("symbolic-ref" (length< (seq-remove (lambda (a) (string-prefix-p "-" a)) rest) 2))
      ("update-index" (and (member "--refresh" rest)
                           (not (member "--add" rest))
                           (not (member "--remove" rest))))
      ("config" (seq-some (lambda (a) (or (string-prefix-p "--get" a)
                                          (member a '("--list" "-l"))))
                          rest))
      ((or "branch" "tag" "remote" "worktree" "stash" "notes")
       (seq-some (lambda (a) (member a '("--list" "-l" "-v" "--verbose" "list" "show"
                                         "get-url" "--contains" "--points-at"
                                         "--merged" "--no-merged")))
                 rest))
      (_ nil))))

(defun fjl/magit-keep-tramp-cache-for-read-only-git (fn destination &rest args)
  "Don't let TRAMP flush its file cache after read-only git commands.
TRAMP flushes everything it knows about the remote host after every
`process-file' unless `process-file-side-effects' is nil, and magit
never binds it. Each refresh then re-stats a dozen files in .git."
  (let ((process-file-side-effects (not (fjl/magit-git-read-only-p args))))
    (apply fn destination args)))

(advice-add 'magit-process-git :around #'fjl/magit-keep-tramp-cache-for-read-only-git)

;;;###autoload
(defun fjl/magit-mode-hook ()
  ;; On macOS, the default git in path is an XCode-enabled wrapper, making it a lot slower
  ;; to invoke. Just calling the regular git exectuable makes magit a bit faster.
  (when (and (not (file-remote-p default-directory)) (eq system-type 'darwin))
    (let ((exe (fjl/mac-git-path)))
      (when (file-exists-p exe)
        (setq-local magit-git-executable exe))))
  (setq-local long-line-threshold nil)
  ;; Bindings
  (define-key magit-mode-map (kbd "#") 'magit-visit-pull-request-url)
  ;; Configure revision buffer.
  (setq magit-revision-insert-related-refs nil)
  ;; Configure status buffer.
  (setq magit-section-initial-visibility-alist '((untracked . hide) (stashes . hide) (unstaged . show) (unpushed . show)))
  (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header)
  ;; Over TRAMP every section is at least one git round trip. Skip the stashes
  ;; section and the recent commits shown when there is nothing to push.
  (when (and (derived-mode-p 'magit-status-mode) (file-remote-p default-directory))
    (setq-local magit-status-sections-hook
                (remq 'magit-insert-stashes
                      (mapcar (lambda (fn)
                                (if (eq fn 'magit-insert-unpushed-to-upstream-or-recent)
                                    'magit-insert-unpushed-to-upstream
                                  fn))
                              magit-status-sections-hook))))
  ;; Configure refs buffer.
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  (remove-hook 'magit-refs-sections-hook 'magit-insert-remote-branches))

;;;###autoload
(add-hook 'magit-mode-hook 'fjl/magit-mode-hook)
