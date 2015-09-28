;; -*- mode: Emacs-Lisp; fill-column: 78; -*-

;; This file contains definitions shared among all init files.
;; These things need to be in their own file because the byte-compiler
;; will not expand/use them correctly when compiling init files
;; individually.

(defconst +fjl-init-lisp+
  (let ((this-file (file-truename (or load-file-name buffer-file-name))))
    (file-name-directory this-file)))
(defconst +fjl-init+
  (file-name-as-directory (expand-file-name (concat +fjl-init-lisp+ ".."))))

(add-to-list 'load-path +fjl-init-lisp+)
(add-to-list 'custom-theme-load-path (concat +fjl-init+ "themes"))
(setq custom-file (concat +fjl-init+ "settings-customize.eld"))

;; Setup package autoloads.
(require 'package)
(setq-default package-user-dir (concat +fjl-init+ "elpa"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(package-initialize)
;; Prevent loading packages twice. All packages are already activated
;; above. package.el will do it again after the init file is loaded
;; unless disabled.
(setq package-enable-at-startup nil)

(defmacro after-package (pkg &rest body)
  "Run `body' when the given package is loaded."
  (declare (indent defun))
  `(eval-after-load ,(symbol-name pkg) (lambda () ,@body)))

(defun make-init-autoloads (&optional skip-if-exists)
  (let ((generated-autoload-file (concat +fjl-init-lisp+ "init-autoloads.el")))
    (unless (and skip-if-exists (file-exists-p generated-autoload-file))
      (update-directory-autoloads +fjl-init-lisp+))))

;; Ensure init-autoloads.el exists so other files can just depend on it.
;; This makes the first startup work without running the makefile.
(make-init-autoloads t)

(provide 'init-bootstrap)
