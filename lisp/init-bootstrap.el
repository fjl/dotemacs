;; -*- lexical-binding: t -*-

;; This file contains definitions shared among all init files. These things need to be in
;; their own file because the byte-compiler will not expand/use them correctly when
;; compiling init files individually.

(defconst +fjl-init-lisp+
  (let ((this-file (file-truename (or load-file-name buffer-file-name))))
    (file-name-directory this-file)))
(defconst +fjl-init+
  (file-name-as-directory (expand-file-name (concat +fjl-init-lisp+ ".."))))

(add-to-list 'load-path +fjl-init-lisp+)
(add-to-list 'custom-theme-load-path (concat +fjl-init+ "themes"))
(setq custom-file (concat +fjl-init+ "settings-customize.eld"))

;; Ensure customize writes 'x instead of (quote x) when saving. You might think this is
;; not important at all, but inconsistencies around this on different platforms and emacs
;; versions cause random changes to the custom file practically every time I change a
;; setting.
(defadvice custom-save-all (around custom-save-fix-quote)
  (let ((print-quoted t))
    ad-do-it))

;; Setup package autoloads.
(require 'package)
(setq-default package-user-dir (file-name-as-directory (concat +fjl-init+ "elpa")))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(add-to-list 'load-path (file-name-as-directory (concat +fjl-init-lisp+ "copilot")))

;; Prevent loading packages twice. All packages are already activated above. package.el
;; will do it again after the init file is loaded unless disabled.
(setq package-enable-at-startup nil)

;; Add vendored packages to the load path.
(dolist (d '("xelb" "exwm" "exim"))
  (add-to-list 'load-path (concat package-user-dir d)))

(defmacro after-package (pkg &rest body)
  "Run `body' when the given package is loaded."
  (declare (indent defun))
  `(eval-after-load ',(symbol-name pkg) (lambda () ,@body)))

(defvar generated-autoload-file)

(defun make-init-autoloads (&optional skip-if-exists)
  (let ((af (concat +fjl-init-lisp+ "init-autoloads.el")))
    (unless (and skip-if-exists (file-exists-p af))
      (if (functionp 'loaddefs-generate)
          (let ((exc
                 (list (concat +fjl-init-lisp+ "init-bootstrap.el")
                       (concat +fjl-init-lisp+ "init-bindings.el")
                       (concat +fjl-init-lisp+ "init-ui.el"))))
            (loaddefs-generate +fjl-init-lisp+ af exc "" nil t))
        ;; Old way for emacs version <= 29
        (progn
          (require 'autoload)
          (let ((generated-autoload-file af))
            (update-directory-autoloads +fjl-init-lisp+)))))))

;; Ensure init-autoloads.el exists so other files can just depend on it. This makes the
;; first startup work without running the makefile.
(make-init-autoloads t)

(provide 'init-bootstrap)
