(require 'go-mode)
(require 'company)
(require 'company-go)
(require 'go-eldoc)
(require 'cl-lib)
(require 's)

(defvar gotools-dir
  (concat user-emacs-directory "gotools")
  "Holds the directory that Go tools are installed in.")

(defvar gotools-gobin
  (concat (file-name-as-directory gotools-dir) "bin")
  "Holds the directory that Go tools binaries are installed in.")

(defvar gotools-list
  '(("godep"     "github.com/tools/godep")
    ("goimports" "github.com/bradfitz/goimports")
    ("gocode"    "github.com/nsf/gocode")
    ("godef"     "github.com/rogpeppe/godef")))

;;;###autoload
(defun gotools-update ()
  "Install go tools in `gotools-dir' and set up various variables to
refer to the installed tools."
  (interactive)
  (make-directory (concat (file-name-as-directory gotools-dir) "src") t)
  (switch-to-buffer "*gotools-update*")
  (erase-buffer)
  (insert
   (propertize
    (format "Installing Go tools.\nGOPATH = %s\nGOBIN = %s\n\n" gotools-dir gotools-gobin)
    'face 'bold))
  (let ((proc nil))
    (let ((process-environment (cl-copy-list process-environment)))
      (setenv "GOPATH" (expand-file-name gotools-dir))
      (setenv "GOBIN" (expand-file-name gotools-gobin))
      (let ((command (cl-list* go-command "get" "-v" "-u" (mapcar #'cadr gotools-list))))
        (setq proc (apply #'start-process "go get" (current-buffer) command))))
    (set-process-sentinel proc (lambda (proc event)
                                 (when (buffer-live-p (process-buffer proc))
                                   (with-current-buffer (process-buffer proc)
                                     (newline)
                                     (insert (process-name proc) " " (propertize event 'face 'bold))
                                     (gotools-setup)))))
    (set-process-query-on-exit-flag proc t)))

;;;###autoload
(defun gotools-setup ()
  "Ensure that the tools installed by `gotools-update' are
present in `exec-path' and the PATH environment variable."
  (when (file-exists-p gotools-gobin)
    (let ((path-list (split-string (getenv "PATH") path-separator))
          (eb (expand-file-name gotools-gobin)))
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
    (when (null dir)
      (let ((gp (fjl/find-gopath)))
        (if (not gp)
            (progn
              (message "Not in any Go workspace.")
              (setq new old))
          ;; A Go workspace was detected.
          (setq dir (car gp))
          (setq new (car gp))
          ;; Add Godep workspace to path if detected.
          (when (second gp)
            (setq new (concat new path-separator (second gp)))))))
    (unless (or (string-equal new old) (fjl/in-goroot-p dir) (fjl/in-godeps-p dir))
      (message (concat "GOPATH = " new))
      (setenv "GOBIN" (concat dir "bin"))
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
  (when (null start) (setq start default-directory))
  (setq start (expand-file-name start))
  (cl-flet ((popdir (path) (file-name-directory (directory-file-name path)))
            (fnd    (path) (file-name-nondirectory (directory-file-name path))))
    (let* ((p              start)
           (name           (fnd start))
           (godep-workspace nil))
      (while (and (not (string-equal p "/")) (not (file-exists-p (concat p "src"))))
        (let ((ws (concat p "Godeps/_workspace/")))
          (when (and (not godep-workspace) (file-exists-p ws))
            (setq godep-workspace ws)))
        (setq p (popdir p))
        (setq name (fnd p)))
      (and (not (string-equal p "/")) (list p godep-workspace)))))

(defun fjl/in-goroot-p (dir)
  (let ((root (s-chomp (shell-command-to-string "go env GOROOT"))))
    (string-prefix-p root (file-name-as-directory dir))))

(defun fjl/in-godeps-p (dir)
  (s-contains? "Godeps/_workspace" dir))

(defun fjl/go-coverage-c.out ()
  (interactive)
  (go-coverage "c.out"))

;; Keys
(define-key go-mode-map (kbd "C-c C-f") 'gofmt)
(define-key go-mode-map (kbd "C-c C-d") 'godef-describe)
(define-key go-mode-map (kbd "C-c d") 'godoc)
(define-key go-mode-map (kbd "M-.") 'godef-jump)
(define-key go-mode-map (kbd "C-x 4 M-.") 'godef-jump-other-window)
(define-key go-mode-map (kbd "M-,") 'pop-tag-mark)
(define-key go-mode-map (kbd "C-c C-i") 'go-goto-imports)
(define-key go-mode-map (kbd "C-c c") 'fjl/go-coverage-c.out)

;;;###autoload
(defun fjl/go-mode-hook ()
  (gopath)
  (gotools-setup)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (go-eldoc-setup)
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode))

;;;###autoload
(add-hook 'go-mode-hook 'fjl/go-mode-hook)

(provide 'init-go)
