;; -*- lexical-binding: t -*-

(require 'go-mode)
(require 'company)
(require 'cl-lib)
(require 's)
(require 'tramp)
(require 'lsp-mode)
(require 'lsp-go)

(defun gotools-dir ()
  "Returns the directory that Go tools should be installed in."
  (let ((remote (file-remote-p default-directory)))
    (file-name-as-directory
     (if remote
         (concat remote user-emacs-directory "gotools")
       (concat user-emacs-directory "gotools")))))

(defun gotools-gobin ()
  "Returns the directory that Go tools binaries are installed in."
  (file-name-as-directory (concat (gotools-dir) "bin")))

(defvar gotools-list
  '(("benchstat"    "golang.org/x/perf/cmd/benchstat")
    ("eg"           "golang.org/x/tools/cmd/eg")
    ("gopls"        "golang.org/x/tools/gopls@v0.4.0")
    ("godep"        "github.com/tools/godep")
    ("godoc"        "golang.org/x/tools/cmd/godoc")
    ("gogetdoc"     "github.com/zmb3/gogetdoc")
    ("goimports"    "golang.org/x/tools/cmd/goimports")
    ("gomvpkg"      "golang.org/x/tools/cmd/gomvpkg")
    ("gorename"     "golang.org/x/tools/cmd/gorename")
    ("govendor"     "github.com/kardianos/govendor")
    ("guru"         "golang.org/x/tools/cmd/guru")
    ("ineffassign"  "github.com/gordonklaus/ineffassign")
    ("pprof"        "github.com/google/pprof")
    ("staticcheck"  "honnef.co/go/tools/cmd/staticcheck")
    ("stress"       "golang.org/x/tools/cmd/stress"))
    ("structlayout" "honnef.co/go/tools/cmd/structlayout"))

(defun fjl/file-name-localname (file)
  (if (tramp-tramp-file-p file)
      (let ((tx (tramp-dissect-file-name file)))
        (tramp-file-name-localname tx))
    file))

;;;###autoload
(defun gotools-update ()
  "Install go tools in `gotools-dir' and set up various variables to
refer to the installed tools."
  (interactive)
  (unless (file-exists-p (gotools-dir))
    (make-directory (gotools-dir)))
  (let* ((modules  (mapcar #'cadr gotools-list))
         (commands (mapcar (lambda (mod) (list (gotools-dir) "go" "get" mod)) modules)))
    (gotools-init-buffer)
    (apply #'gotools-run-commands commands)))

;;;###autoload
(defun gotools-rebuild ()
  "Rebuild existing go tools in `gotools-dir' and set up various variables to
refer to the installed tools."
  (interactive)
  (dolist (cmd gotools-list)
    (let ((file (concat (gotools-gobin) (car cmd))))
      (when (file-exists-p file)
        (delete-file file t))))
  (gotools-init-buffer)
  (gotools-run-commands
   (cl-list* (gotools-dir) "go" "install" "-v" (mapcar #'cadr gotools-list))))

(defun gotools-init-buffer ()
  (pop-to-buffer-same-window "*gotools-update*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert
     (propertize (format "Installing Go tools to %s\n\n" (gotools-dir)) 'face 'bold)))
  (setq buffer-read-only t)
  (view-mode 1))

(defun gotools-run-commands (&rest commands)
  (let* ((gopath (fjl/file-name-localname (expand-file-name (gotools-dir))))
         (gobin  (fjl/file-name-localname (expand-file-name (gotools-gobin))))
         (command (car commands))
         (default-directory (car command))
         (exe (cadr command))
         (args (cddr command))
         (proc nil))
    (let ((process-environment (cl-list* "GO111MODULE=on"
                                         "TERM=dumb"
                                         (concat "GOPATH=" gopath)
                                         (concat "GOBIN=" gobin) process-environment)))
      (let ((inhibit-read-only t))
        (insert (s-join " " (cdr command)))
        (newline))
      (setq proc (apply #'start-file-process exe (current-buffer) exe args))
      (set-process-sentinel proc (lambda (proc event)
                                   (when (buffer-live-p (process-buffer proc))
                                     (with-current-buffer (process-buffer proc)
                                       (let ((inhibit-read-only t))
                                         (cl-fresh-line)
                                         (insert exe " " (car args) " " (propertize event 'face 'bold))
                                         (newline))
                                       ;; Start next command.
                                       (if (and (cdr commands) (equal event "finished\n"))
                                           (apply #'gotools-run-commands (cdr commands))
                                         (gotools-setup))))))
      (set-process-query-on-exit-flag proc t))))

;;;###autoload
(defun gotools-setup ()
  "Ensure that the tools installed by `gotools-update' are
present in `exec-path' and the PATH environment variable."
  (interactive)
  (when (and (file-exists-p (gotools-gobin)) (not (file-remote-p (gotools-gobin))))
    (let* ((path-list (split-string (getenv "PATH") path-separator))
           (eb        (expand-file-name (gotools-gobin)))
           (goimports (concat (file-name-as-directory eb) "goimports")))
      (setq-default gofmt-command goimports)
      (add-to-list 'exec-path eb)
      (unless (member eb path-list)
        (setenv "PATH" (concat eb path-separator (getenv "PATH")))))))

;;;###autoload
(defun gopath (&optional dir)
  "Set GOPATH in `process-environment' to DIR. If DIR is
not specified or nil, the Go workspace root is auto-detected
from the directory of the current buffer.

When invoked interactively with a prefix argument,
the workspace root directory is read from the minibuffer.

Returns the new value of GOPATH."
  (interactive
   (when current-prefix-arg
     (list (read-directory-name "GOPATH: " (getenv "GOPATH")))))
  (let ((old (getenv "GOPATH"))
        (new dir))
    (if (not (null dir))
        (setq new (fjl/file-name-localname dir))
      (let ((gp (fjl/find-gopath)))
        (if (not gp)
            (progn
              (message "Not in any Go workspace.")
              (setq new old))
          ;; A Go workspace was detected.
          (setq dir (car gp))
          (setq new (fjl/file-name-localname (car gp)))
          ;; Add Godep workspace to path if detected.
          (when (cadr gp)
            (setq new (concat new path-separator (fjl/file-name-localname (cadr gp))))))))
    (unless (or (string-equal new old) (fjl/in-goroot-p dir) (fjl/in-godeps-p dir))
      (message (concat "GOPATH = " new))
      (setenv "GOBIN" (concat (file-name-as-directory (car (split-string dir "[:;]"))) "bin"))
      (setenv "GOPATH" new))))

;;;###autoload
(defun gocd (package)
  (interactive "MPackage: " )
  (let* ((exp    (shell-command-to-string (concat "go list .../" package)))
         (gopath (file-name-as-directory (getenv "GOPATH")))
         (dir    (concat gopath "src/" (s-chomp exp))))
    (if (not (file-exists-p dir))
        (message "Not found: %s" (s-chomp exp))
      (message "cd %s" dir)
      (cd dir))))

(defun fjl/find-gopath (&optional start)
  "Auto-detect a Go workspace root, starting at directory START.
The second return value is the path to the innermost Godep
workspace above START or nil if no Godep workspace directory was
found."
  (let ((godep-workspace nil))
    (let ((gopath (locate-dominating-file
                   (or start ".")
                   (lambda (dir)
                     (let ((ws (concat dir "Godeps/_workspace/")))
                       (when (and (not godep-workspace) (file-exists-p ws))
                         (setq godep-workspace (expand-file-name ws))))
                     (file-exists-p (concat dir "src"))))))
      (and gopath (list (expand-file-name gopath) godep-workspace)))))

(defun fjl/in-goroot-p (dir)
  (let ((root (s-chomp (shell-command-to-string "go env GOROOT"))))
    (string-prefix-p root (file-name-as-directory dir))))

(defun fjl/in-godeps-p (dir)
  (s-contains? "Godeps/_workspace" dir))

(defun fjl/go-coverage-c.out ()
  (interactive)
  (go-coverage "c.out"))

(defun go-toggle-initializer (&optional rec)
  "Moves a variable initialization in or out of the head part of an if or switch."
  (interactive)
  (if (fjl/go-condition-head-p)
    ;; If we're on a line with a condition, modify its head.
    (atomic-change-group
      (beginning-of-line)
      (forward-word)
      (let ((start (point))
            (semi  (fjl/search-forward-no-comment ";" (line-end-position))))
        (if semi
          ;; Move out the existing initializer.
          (let ((init (fjl/delete-region-string start semi)))
            (forward-line -1)
            (end-of-line)
            (newline-and-indent)
            (insert (s-chop-suffix ";" (s-trim init))))
          ;; The condition has no initializer. Pull in the previous
          ;; line if it contains code. Kill any comment on the
          ;; previous line because it doesn't fit into the header.
          (unless (fjl/boring-go-line-p 0)
            (save-excursion
              (forward-line -1)
              (comment-kill nil))
            (let ((init (fjl/delete-region-string (line-beginning-position 0) (1+ (line-end-position 0)))))
              (insert " ")
              (insert (s-chop-suffix ";" (s-trim init)))
              (insert ";"))))))
    (when (and (fjl/go-condition-head-p 2) (not rec))
      ;; If the line after the current one is a condition,
      ;; do the edit from inside its head instead.
      (forward-line 1)
      (go-toggle-initializer t))))

(defun fjl/boring-go-line-p (n)
  (let ((line (s-trim (buffer-substring (line-beginning-position n) (line-end-position n)))))
    (or (s-blank? line) (s-prefix-p "/*" line) (s-prefix-p "//" line))))

(defun fjl/go-condition-head-p (&optional n)
  (string-match-p "[[:space:]]*\\(if\\|switch\\|for\\)\\b"
                  (buffer-substring (line-beginning-position n) (line-end-position n))))

(defun fjl/search-forward-no-comment (what bound)
  "Like `search-forward' but skips any matches inside of a string or comment."
  (cl-loop for m = (search-forward what bound t)
           for s = (and m (syntax-ppss m))
           when (or (null m) (and (not (nth 3 s)) (not (nth 4 s))))
           return m))

(defun fjl/delete-region-string (start end)
  (prog1 (buffer-substring start end)
    (delete-region start end)))

;; Keys
(define-key go-mode-map (kbd "C-c i") 'go-toggle-initializer)
(define-key go-mode-map (kbd "C-c d") 'godoc)
(define-key go-mode-map (kbd "C-c C-i") 'go-goto-imports)
(define-key go-mode-map (kbd "C-c c") 'fjl/go-coverage-c.out)
(define-key go-mode-map (kbd "C-c r") 'lsp-rename)

;;;###autoload
(defun fjl/go-mode-hook ()
  (gopath)
  (gotools-setup)
  (prettify-symbols-mode)
  (company-mode 1)
  (lsp-deferred)
  (add-hook 'before-save-hook 'lsp-format-buffer)
  (add-hook 'before-save-hook 'lsp-organize-imports))

;;;###autoload
(add-hook 'go-mode-hook 'fjl/go-mode-hook)

(provide 'init-go)
