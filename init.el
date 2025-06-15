;;  -*- lexical-binding: t; -*-

;; Added by Package.el.
;(package-initialize)

;; (push (lambda (f) (message "loading %s" f)) after-load-functions)

;; Don't warn unless it's serious.
;; (eval-when-compile (require 'warnings))
;; (setq warning-minimum-level :error)

;; Inhibit GC during initialization.
(setq gc-cons-threshold (* 50 1024 1024))

;; Raise read-process-output-max. This is supposed to improve performance.
(setq read-process-output-max 1048576)
(setq process-adaptive-read-buffering nil)

;; Load source instead of bytecode if the bytecode is outdated.
(setq load-prefer-newer t)
;; Don't warn if .emacs.d is not writable.
(setq user-emacs-directory-warning nil)

(let* ((init-dir (file-name-directory (file-truename (or load-file-name buffer-file-name))))
       (file-name-handler-alist nil))
  ;; Load the bootstrap file. This sets up all load paths and activates all packages.
  (load (concat init-dir "lisp/init-bootstrap"))
  ;; Apply UI customizations first so the display doesn't jump so much.
  (require 'init-ui)
  ;; Load init files.
  (load custom-file nil t)
  (require 'init-commands)
  (require 'init-bindings)
  ;; Enable some built-in packages.
  (require 'uniquify)
  ;; Enable server for GUI emacs.
  (unless (eq t (framep-on-display))
    (server-mode 1)))

;; Set EDITOR for all subprocesses.
(setenv "EDITOR" "emacsclient")

;; Set GC trigger back to a reasonable value after initializing everything.
(setq gc-cons-threshold (* 12 1024 1024))

;; Disable certain local variables.
(dolist (v '(Package Base Syntax))
  (add-to-list 'ignored-local-variables v))

;; Enable some disabled commands. This goes last because emacs adds
;; them to the end of this file when enabling interactively.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'timer-list 'disabled nil)
(put 'list-timers 'disabled nil)
