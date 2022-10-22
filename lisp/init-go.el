;; -*- lexical-binding: t -*-

(require 'go-mode)
(require 'company)
(require 'cl-lib)
(require 's)
(require 'tramp)
(require 'eglot)

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
  '(("gopls"        "golang.org/x/tools/gopls@latest")
    ("benchstat"    "golang.org/x/perf/cmd/benchstat@latest")
    ("eg"           "golang.org/x/tools/cmd/eg@latest")
    ("stringer"     "golang.org/x/tools/cmd/stringer@latest")
    ("godef"        "github.com/rogpeppe/godef@latest")
    ("godoc"        "golang.org/x/tools/cmd/godoc@latest")
    ("gogetdoc"     "github.com/zmb3/gogetdoc@latest")
    ("goimports"    "golang.org/x/tools/cmd/goimports@latest")
    ("gomvpkg"      "golang.org/x/tools/cmd/gomvpkg@latest")
    ("gorename"     "golang.org/x/tools/cmd/gorename@latest")
    ("ineffassign"  "github.com/gordonklaus/ineffassign@latest")
    ("pprof"        "github.com/google/pprof@latest")
    ("staticcheck"  "honnef.co/go/tools/cmd/staticcheck@latest")
    ("stress"       "golang.org/x/tools/cmd/stress@latest")
    ("structlayout" "honnef.co/go/tools/cmd/structlayout@latest")
    ("structlayout-pretty"   "honnef.co/go/tools/cmd/structlayout-pretty@latest")
    ("structlayout-optimize" "honnef.co/go/tools/cmd/structlayout-optimize@latest")))

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
         (commands (mapcar (lambda (mod) (list (gotools-dir) "go" "install" mod)) modules)))
    (gotools-init-buffer)
    (apply #'gotools-run-commands commands)))

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
                                       (save-excursion
                                         (let ((inhibit-read-only t))
                                           (goto-char (point-max))
                                           (cl-fresh-line)
                                           (insert exe " " (car args) " " (propertize event 'face 'bold) "\n"))
                                         ;; Start next command.
                                         (if (and (cdr commands) (equal event "finished\n"))
                                             (apply #'gotools-run-commands (cdr commands))
                                           (gotools-setup)))))))
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
            (setq new old)
          ;; A Go workspace was detected.
          (setq dir gp)
          (setq new (fjl/file-name-localname gp)))))
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

(defvar fjl/definitely-not-gopath '("/" "/usr" "/usr/local"))

(defun fjl/find-gopath (&optional start)
  "Auto-detect a Go workspace root, starting at directory START."
  (let ((gopath
         (locate-dominating-file
          (or start ".")
          (lambda (dir)
            (and (file-exists-p (concat dir "src"))
                 (not (member dir fjl/definitely-not-gopath)))))))
    (and gopath (expand-file-name gopath))))

(defun fjl/in-goroot-p (dir)
  (let ((root (s-chomp (shell-command-to-string "go env GOROOT"))))
    (string-prefix-p root (file-name-as-directory dir))))

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
(define-key go-mode-map (kbd "C-c r") 'eglot-rename)

(defun fjl/eglot-go-server (&optional arg)
  "This function finds gopls for use by eglot."
  (let ((gopls-in-tools (concat (gotools-gobin) "gopls")))
    (if (file-exists-p gopls-in-tools)
        (list (fjl/file-name-localname (expand-file-name gopls-in-tools)))
      (list "gopls"))))

(setf (cdr (assoc 'go-mode eglot-server-programs)) #'fjl/eglot-go-server)

(defun fjl/eglot-go-before-save ()
  (eglot-code-action-organize-imports (point-min) (point-max))
  (eglot-format-buffer))

;;;###autoload
(defun fjl/go-mode-hook ()
  (gopath)
  (gotools-setup)
  (prettify-symbols-mode)
  (company-mode 1)
  (eglot-ensure)
  (add-hook 'before-save-hook 'fjl/eglot-go-before-save))

;;;###autoload
(add-hook 'go-mode-hook 'fjl/go-mode-hook)

(provide 'init-go)
