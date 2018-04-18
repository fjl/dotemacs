;;; with-editor-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "with-editor" "with-editor.el" (0 0 0 0))
;;; Generated autoloads from with-editor.el

(autoload 'with-editor-export-editor "with-editor" "\
Teach subsequent commands to use current Emacs instance as editor.\n\nSet and export the environment variable ENVVAR, by default\n\"EDITOR\".  The value is automatically generated to teach\ncommands to use the current Emacs instance as \"the editor\".\n\nThis works in `shell-mode', `term-mode' and `eshell-mode'.\n\n(fn &optional (ENVVAR \"EDITOR\"))" t nil)

(autoload 'with-editor-export-git-editor "with-editor" "\
Like `with-editor-export-editor' but always set `$GIT_EDITOR'.\n\n(fn)" t nil)

(autoload 'with-editor-export-hg-editor "with-editor" "\
Like `with-editor-export-editor' but always set `$HG_EDITOR'.\n\n(fn)" t nil)

(defvar shell-command-with-editor-mode nil "\
Non-nil if Shell-Command-With-Editor mode is enabled.\nSee the `shell-command-with-editor-mode' command\nfor a description of this minor mode.")

(custom-autoload 'shell-command-with-editor-mode "with-editor" nil)

(autoload 'shell-command-with-editor-mode "with-editor" "\
Teach `shell-command' to use current Emacs instance as editor.\n\nTeach `shell-command', and all commands that ultimately call that\ncommand, to use the current Emacs instance as editor by executing\n\"EDITOR=CLIENT COMMAND&\" instead of just \"COMMAND&\".\n\nCLIENT is automatically generated; EDITOR=CLIENT instructs\nCOMMAND to use to the current Emacs instance as \"the editor\",\nassuming no other variable overrides the effect of \"$EDITOR\".\nCLIENT may be the path to an appropriate emacsclient executable\nwith arguments, or a script which also works over Tramp.\n\nAlternatively you can use the `with-editor-async-shell-command',\nwhich also allows the use of another variable instead of\n\"EDITOR\".\n\n(fn &optional ARG)" t nil)

(autoload 'with-editor-async-shell-command "with-editor" "\
Like `async-shell-command' but with `$EDITOR' set.\n\nExecute string \"ENVVAR=CLIENT COMMAND\" in an inferior shell;\ndisplay output, if any.  With a prefix argument prompt for an\nenvironment variable, otherwise the default \"EDITOR\" variable\nis used.  With a negative prefix argument additionally insert\nthe COMMAND's output at point.\n\nCLIENT is automatically generated; ENVVAR=CLIENT instructs\nCOMMAND to use to the current Emacs instance as \"the editor\",\nassuming it respects ENVVAR as an \"EDITOR\"-like variable.\nCLIENT may be the path to an appropriate emacsclient executable\nwith arguments, or a script which also works over Tramp.\n\nAlso see `async-shell-command' and `shell-command'.\n\n(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER ENVVAR)" t nil)

(autoload 'with-editor-shell-command "with-editor" "\
Like `shell-command' or `with-editor-async-shell-command'.\nIf COMMAND ends with \"&\" behave like the latter,\nelse like the former.\n\n(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER ENVVAR)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "with-editor" '("with-editor" "start-file-process--with-editor-process-filter" "server-" "shell-command--shell-command-with-editor-mode")))

;;;***

;;;### (autoloads nil nil ("with-editor-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; with-editor-autoloads.el ends here
