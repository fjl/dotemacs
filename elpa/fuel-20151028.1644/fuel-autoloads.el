;;; fuel-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "factor-mode" "factor-mode.el" (22108 12363
;;;;;;  0 0))
;;; Generated autoloads from factor-mode.el

(let ((loads (get 'factor 'custom-loads))) (if (member '"factor-mode" loads) nil (put 'factor 'custom-loads (cons '"factor-mode" loads))))

(let ((loads (get 'factor-faces 'custom-loads))) (if (member '"factor-mode" loads) nil (put 'factor-faces 'custom-loads (cons '"factor-mode" loads))))

(autoload 'factor-mode "factor-mode" "\
A mode for editing programs written in the Factor programming language.
\\{factor-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.factor\\'" . factor-mode))

(add-to-list 'interpreter-mode-alist '("factor" . factor-mode))

;;;***

;;;### (autoloads nil "fuel-autodoc" "fuel-autodoc.el" (22108 12363
;;;;;;  0 0))
;;; Generated autoloads from fuel-autodoc.el

(let ((loads (get 'fuel-autodoc 'custom-loads))) (if (member '"fuel-autodoc" loads) nil (put 'fuel-autodoc 'custom-loads (cons '"fuel-autodoc" loads))))

(autoload 'fuel-autodoc-mode "fuel-autodoc" "\
Toggle Fuel's Autodoc mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Autodoc mode is enabled, a synopsis of the word at point is
displayed in the minibuffer.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "fuel-autohelp" "fuel-autohelp.el" (22108 12363
;;;;;;  0 0))
;;; Generated autoloads from fuel-autohelp.el

(let ((loads (get 'fuel-autohelp 'custom-loads))) (if (member '"fuel-autohelp" loads) nil (put 'fuel-autohelp 'custom-loads (cons '"fuel-autohelp" loads))))

(autoload 'fuel-autohelp-mode "fuel-autohelp" "\
Toggle Fuel's Autohelp mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Autohelp mode is enabled, the help for the word is displayed
in another window.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-fuel-autohelp-mode "fuel-autohelp" "\
Unequivocally turn on FUEL's Autohelp mode (see command
`fuel-autohelp-mode').

\(fn)" t nil)

;;;***

;;;### (autoloads nil "fuel-base" "fuel-base.el" (22108 12363 0 0))
;;; Generated autoloads from fuel-base.el

(defsubst fuel-version nil "\
Echoes FUEL's version." (interactive) (message "FUEL %s" fuel-version))

(let ((loads (get 'fuel 'custom-loads))) (if (member '"fuel-base" loads) nil (put 'fuel 'custom-loads (cons '"fuel-base" loads))))

;;;***

;;;### (autoloads nil "fuel-debug" "fuel-debug.el" (22108 12363 0
;;;;;;  0))
;;; Generated autoloads from fuel-debug.el

(let ((loads (get 'fuel-debug 'custom-loads))) (if (member '"fuel-debug" loads) nil (put 'fuel-debug 'custom-loads (cons '"fuel-debug" loads))))

(autoload 'fuel-debug-mode "fuel-debug" "\
A major mode for displaying Factor's compilation results and
invoking restarts as needed.
\\{fuel-debug-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "fuel-debug-uses" "fuel-debug-uses.el" (22108
;;;;;;  12363 0 0))
;;; Generated autoloads from fuel-debug-uses.el

(let ((loads (get 'fuel-debug-uses 'custom-loads))) (if (member '"fuel-debug-uses" loads) nil (put 'fuel-debug-uses 'custom-loads (cons '"fuel-debug-uses" loads))))

(autoload 'fuel-debug-uses-mode "fuel-debug-uses" "\
A major mode for displaying Factor's USING: inference results.
\\{fuel-debug-uses-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "fuel-edit" "fuel-edit.el" (22108 12363 0 0))
;;; Generated autoloads from fuel-edit.el

(autoload 'fuel-edit-vocabulary "fuel-edit" "\
Visits vocabulary file in Emacs.
When called interactively, asks for vocabulary with completion.
With prefix argument, refreshes cached vocabulary list.

\(fn &optional REFRESH VOCAB)" t nil)

(autoload 'fuel-edit-word "fuel-edit" "\
Asks for a word to edit, with completion.
With prefix, only words visible in the current vocabulary are
offered.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "fuel-help" "fuel-help.el" (22108 12363 0 0))
;;; Generated autoloads from fuel-help.el

(let ((loads (get 'fuel-help 'custom-loads))) (if (member '"fuel-help" loads) nil (put 'fuel-help 'custom-loads (cons '"fuel-help" loads))))

(autoload 'fuel-help-mode "fuel-help" "\
Major mode for browsing Factor documentation.
\\{fuel-help-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "fuel-listener" "fuel-listener.el" (22108 12363
;;;;;;  0 0))
;;; Generated autoloads from fuel-listener.el

(let ((loads (get 'fuel-listener 'custom-loads))) (if (member '"fuel-listener" loads) nil (put 'fuel-listener 'custom-loads (cons '"fuel-listener" loads))))

(autoload 'run-factor "fuel-listener" "\
Show the fuel-listener buffer, starting the process if needed.

\(fn &optional ARG)" t nil)

(autoload 'connect-to-factor "fuel-listener" "\
Connects to a remote listener running in the same host.
Without prefix argument, the default port, 9000, is used.
Otherwise, you'll be prompted for it. To make this work, in the
remote listener you need to issue the words
'fuel-start-remote-listener*' or 'port
fuel-start-remote-listener', from the fuel vocabulary.

\(fn &optional ARG)" t nil)

(autoload 'fuel-listener-mode "fuel-listener" "\
Major mode for interacting with an inferior Factor listener process.
\\{fuel-listener-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "fuel-log" "fuel-log.el" (22108 12363 0 0))
;;; Generated autoloads from fuel-log.el

(autoload 'factor-messages-mode "fuel-log" "\
Simple mode to log interactions with the factor listener

\(fn)" t nil)

;;;***

;;;### (autoloads nil "fuel-mode" "fuel-mode.el" (22108 12363 0 0))
;;; Generated autoloads from fuel-mode.el

(let ((loads (get 'fuel-mode 'custom-loads))) (if (member '"fuel-mode" loads) nil (put 'fuel-mode 'custom-loads (cons '"fuel-mode" loads))))

(autoload 'fuel-mode "fuel-mode" "\
Toggle Fuel's mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Fuel mode is enabled, a host of nice utilities for
interacting with a factor listener is at your disposal.
\\{fuel-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "fuel-popup" "fuel-popup.el" (22108 12363 0
;;;;;;  0))
;;; Generated autoloads from fuel-popup.el

(autoload 'fuel-popup-mode "fuel-popup" "\
Mode for displaying read only stuff

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "fuel-scaffold" "fuel-scaffold.el" (22108 12363
;;;;;;  0 0))
;;; Generated autoloads from fuel-scaffold.el

(let ((loads (get 'fuel-scaffold 'custom-loads))) (if (member '"fuel-scaffold" loads) nil (put 'fuel-scaffold 'custom-loads (cons '"fuel-scaffold" loads))))

(autoload 'fuel-scaffold-vocab "fuel-scaffold" "\
Creates a directory in the given root for a new vocabulary and
adds source and authors.txt files. Prompts the user for optional summary,
tags, help, and test file creation.

You can configure `fuel-scaffold-developer-name' for the name to
be inserted in the generated files.

\(fn &optional OTHER-WINDOW NAME-HINT ROOT-HINT)" t nil)

(autoload 'fuel-scaffold-help "fuel-scaffold" "\
Creates, if it does not already exist, a help file with
scaffolded help for each word in the current vocabulary.

With prefix argument, ask for the vocabulary name. You can
configure `fuel-scaffold-developer-name' for the name to be
inserted in the generated file.

\(fn &optional ARG)" t nil)

(autoload 'fuel-scaffold-tests "fuel-scaffold" "\
Creates, if it does not already exist, a tests file for the current
vocabulary.

With prefix argument, ask for the vocabulary name. You can
configure `fuel-scaffold-developer-name' for the name to be
inserted in the generated file.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "fuel-stack" "fuel-stack.el" (22108 12363 0
;;;;;;  0))
;;; Generated autoloads from fuel-stack.el

(let ((loads (get 'fuel-stack 'custom-loads))) (if (member '"fuel-stack" loads) nil (put 'fuel-stack 'custom-loads (cons '"fuel-stack" loads))))

(autoload 'fuel-stack-mode "fuel-stack" "\
Toggle Fuel's Stack mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Stack mode is enabled, inferred stack effects for current
sexp are automatically displayed in the echo area.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "fuel-xref" "fuel-xref.el" (22108 12363 0 0))
;;; Generated autoloads from fuel-xref.el

(let ((loads (get 'fuel-xref 'custom-loads))) (if (member '"fuel-xref" loads) nil (put 'fuel-xref 'custom-loads (cons '"fuel-xref" loads))))

(autoload 'fuel-xref-mode "fuel-xref" "\
Mode for displaying FUEL cross-reference information.
\\{fuel-xref-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("fuel-completion.el" "fuel-connection.el"
;;;;;;  "fuel-eval.el" "fuel-markup.el" "fuel-menu.el" "fuel-pkg.el"
;;;;;;  "fuel-refactor.el" "fuel-table.el" "fuel-tests.el") (22108
;;;;;;  12363 484586 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; fuel-autoloads.el ends here
