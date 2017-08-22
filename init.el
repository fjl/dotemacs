;; Added by Package.el.
;(package-initialize)

;; (push (lambda (f) (message "loading %s" f)) after-load-functions)

;; Don't warn unless it's serious.
;; (eval-when-compile (require 'warnings))
;; (setq warning-minimum-level :error)

;; Inhibit GC during initialization.
(defvar prev-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold (* 20 1024 1024))
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
  (require 'uniquify))

;; Setup an autoload for mu4e because it doesn't have a package.
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(when (cl-some (lambda (d)
                 (file-exists-p (concat (file-name-as-directory d) "mu4e.el")))
               load-path)
  (autoload 'mu4e "mu4e" "If mu4e is not running yet, start it." t)
  (autoload 'mu4e-user-agent "mu4e" nil nil)
  (eval-after-load 'mu4e '(require 'init-mu4e))
  (setq mail-user-agent 'mu4e-user-agent))

;; Set EDITOR for all subprocesses.
(setenv "EDITOR" "emacsclient")

;; Set GC trigger back to a reasonable value after initializing everything.
(setq gc-cons-threshold prev-gc-cons-threshold)

;; Enable some disabled commands. This goes last because emacs adds
;; them to the end of this file when enabling interactively.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'timer-list 'disabled nil)
