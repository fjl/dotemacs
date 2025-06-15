;;  -*- lexical-binding: t; -*-

;; Prevent loading packages twice. All packages are already activated in
;; lisp/init-bootstrap.el. package.el will do it again after the init file is loaded
;; unless disabled.
(setq package-enable-at-startup nil)

;; Set up library path for libgccjit. This is a workaround for issues
;; with native elisp compilation on macOS.
(when (and (eq system-type 'darwin) (featurep 'native-compile))
  (defvar native-comp-driver-options nil)
  (push "-L/opt/homebrew/lib/gcc/current" native-comp-driver-options))
