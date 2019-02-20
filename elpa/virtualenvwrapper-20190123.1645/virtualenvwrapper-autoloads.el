;;; virtualenvwrapper-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "virtualenvwrapper" "virtualenvwrapper.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from virtualenvwrapper.el

(autoload 'venv-projectile-auto-workon "virtualenvwrapper" "\
If a venv in the projetile root exists, activates it.\nSet your common venvs names in `venv-dirlookup-names'\n\n(fn)" nil nil)

(autoload 'venv-deactivate "virtualenvwrapper" "\
Deactivate the current venv.\n\n(fn)" t nil)

(autoload 'venv-set-location "virtualenvwrapper" "\
Set where to look for virtual environments to LOCATION.\nThis is useful e.g. when using tox.\n\n(fn &optional LOCATION)" t nil)

(autoload 'venv-workon "virtualenvwrapper" "\
Interactively switch to virtualenv NAME. Prompts for name if called\ninteractively.\n\n(fn &optional NAME)" t nil)

(autoload 'venv-mkvirtualenv-using "virtualenvwrapper" "\
Create new virtualenvs NAMES using INTERPRETER. If venv-location\nis a single directory, the new virtualenvs are made there; if it\nis a list of directories, the new virtualenvs are made in the\ncurrent `default-directory'.\n\n(fn INTERPRETER &rest NAMES)" t nil)

(autoload 'venv-mkvirtualenv "virtualenvwrapper" "\
Create new virtualenvs NAMES. If venv-location is a single\ndirectory, the new virtualenvs are made there; if it is a list of\ndirectories, the new virtualenvs are made in the current\n`default-directory'.\n\n(fn &rest NAMES)" t nil)

(autoload 'venv-rmvirtualenv "virtualenvwrapper" "\
Delete virtualenvs NAMES.\n\n(fn &rest NAMES)" t nil)

(autoload 'venv-lsvirtualenv "virtualenvwrapper" "\
List all available virtualenvs in a temp buffer.\n\n(fn)" t nil)

(autoload 'venv-cdvirtualenv "virtualenvwrapper" "\
Change to the directory of current virtualenv. If\nSUBDIR is passed, append that to the path such that\nwe are immediately in that directory.\n\n(fn &optional SUBDIR)" t nil)

(autoload 'venv-cpvirtualenv "virtualenvwrapper" "\
Copy virtualenv NAME to NEWNAME. Any arguments not passed will be\nprompted for This comes with the same caveat as cpvirtualenv in the\noriginal virtualenvwrapper, which is that is far from guarenteed to\nwork well. Many packages hardcode absolute paths in various places an\nwill break if moved to a new location. Use with caution. If used with\na single virtualenv directory, behaves just like cpvirtualenv in\nvirtualenvwrapper.sh.  If used with virtualenvs spread around the\nfilesystem, creates the new virtualenv in the current default\ndirectory.\n\n(fn &optional NAME NEWNAME)" t nil)

(autoload 'venv-shell-init "virtualenvwrapper" "\
Activate the current virtualenv in a newly opened shell.\n\n(fn PROCESS)" nil nil)

(autoload 'venv-initialize-interactive-shells "virtualenvwrapper" "\
Configure interactive shells for use with\nvirtualenvwrapper.el.\n\n(fn)" nil nil)

(autoload 'venv-initialize-eshell "virtualenvwrapper" "\
Configure eshell for use with virtualenvwrapper.el.\n\n(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "virtualenvwrapper" '("venv-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; virtualenvwrapper-autoloads.el ends here
