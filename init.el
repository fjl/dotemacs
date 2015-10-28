;; -*- mode: Emacs-Lisp; fill-column: 78; -*-

;; (push (lambda (f) (message "loading %s" f)) after-load-functions)

;; Don't warn unless it's serious.
(eval-when-compile (require 'warnings))
(setq warning-minimum-level :error)

;; Load the bootstrap file. This sets up all load paths and activates all packages.
(let* ((init-dir (file-name-directory (file-truename (or load-file-name buffer-file-name))))
       (init-lisp-dir (concat init-dir "lisp/")))
  (add-to-list 'load-path init-lisp-dir)
  (require 'init-bootstrap))

;; Apply UI customizations first so the display doesn't jump so much.
(require 'init-ui)

;; Load init files.
(load custom-file)
(require 'init-commands)
(require 'init-bindings)

;; Enable some built-in packages.
(require 'uniquify)

;; Set PATH on OS X. This is necessary because Emacs.app
;; is started by launchd and does not have the shell environment.
(when (eq system-type 'darwin)
  (let* ((homebin (expand-file-name "~/bin"))
         (path `(,homebin
                 "/usr/local/bin" "/usr/local/sbin"
                 "/usr/bin" "/usr/sbin"
                 "/bin" "/sbin"))
         (cpath ""))
    (dolist (dir path)
      (setq cpath (concat cpath (if (cl-plusp (length cpath)) ":") dir)))
    (setq exec-path path)
    (setenv "PATH" cpath)))

;; Setup an autoload for mu4e because it doesn't have a package.
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(when (cl-some (lambda (d)
                 (file-exists-p (concat (file-name-as-directory d) "mu4e.el")))
               load-path)
  (autoload 'mu4e "mu4e" "If mu4e is not running yet, start it." t)
  (autoload 'mu4e-user-agent "mu4e" nil nil)
  (eval-after-load 'mu4e '(require 'init-mu4e))
  (setq mail-user-agent 'mu4e-user-agent))

;; Enable some disabled commands. This goes last because emacs adds
;; them to the end of this file when enabling interactively.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
