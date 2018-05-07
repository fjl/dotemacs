;;; erlang-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "erlang" "erlang.el" (0 0 0 0))
;;; Generated autoloads from erlang.el

(autoload 'erlang-mode "erlang" "\
Major mode for editing Erlang source files in Emacs.\nIt knows about syntax and comment, it can indent code, it is capable\nof fontifying the source file, the TAGS commands are aware of Erlang\nmodules, and the Erlang man pages can be accessed.\n\nShould this module, \"erlang.el\", be installed properly, Erlang mode\nis activated whenever an Erlang source or header file is loaded into\nEmacs.  To indicate this, the mode line should contain the word\n\"Erlang\".\n\nThe main feature of Erlang mode is indentation, press TAB and the\ncurrent line will be indented correctly.\n\nComments starting with only one `%' are indented to the column stored\nin the variable `comment-column'.  Comments starting with two `%':s\nare indented with the same indentation as code.  Comments starting\nwith at least three `%':s are indented to the first column.\n\nHowever, Erlang mode contains much more, this is a list of the most\nuseful commands:\n     TAB     - Indent the line.\n     C-c C-q - Indent current function.\n     M-;     - Create a comment at the end of the line.\n     M-q     - Fill a comment, i.e. wrap lines so that they (hopefully)\n                 will look better.\n     M-a     - Goto the beginning of an Erlang clause.\n     M-C-a   - Ditto for function.\n     M-e     - Goto the end of an Erlang clause.\n     M-C-e   - Ditto for function.\n     M-h     - Mark current Erlang clause.\n     M-C-h   - Ditto for function.\n     C-c C-z - Start, or switch to, an inferior Erlang shell.\n     C-c C-k - Compile current file.\n     C-x `   - Next error.\n     ,       - Electric comma.\n     ;       - Electric semicolon.\n\nErlang mode check the name of the file against the module name when\nsaving, whenever a mismatch occurs Erlang mode offers to modify the\nsource.\n\nThe variable `erlang-electric-commands' controls the electric\ncommands.  To deactivate all of them, set it to nil.\n\nThere exists a large number of commands and variables in the Erlang\nmodule.  Please press `M-x apropos RET erlang RET' to see a complete\nlist.  Press `C-h f name-of-function RET' and `C-h v name-of-variable\nRET'to see the full description of functions and variables,\nrespectively.\n\nOn entry to this mode the contents of the hook `erlang-mode-hook' is\nexecuted.\n\nPlease see the beginning of the file `erlang.el' for more information\nand examples of hooks.\n\nOther commands:\n\\{erlang-mode-map}\n\n(fn)" t nil)

(dolist (r '("\\.erl$" "\\.app\\.src$" "\\.escript" "\\.hrl$" "\\.xrl$" "\\.yrl" "/ebin/.+\\.app")) (add-to-list 'auto-mode-alist (cons r 'erlang-mode)))

(autoload 'erlang-find-tag "erlang" "\
Like `find-tag'.  Capable of retrieving Erlang modules.\n\nTags can be given on the forms `tag', `module:', `module:tag'.\n\n(fn MODTAGNAME &optional NEXT-P REGEXP-P)" t nil)

(autoload 'erlang-find-tag-other-window "erlang" "\
Like `find-tag-other-window' but aware of Erlang modules.\n\n(fn TAGNAME &optional NEXT-P REGEXP-P)" t nil)

(autoload 'erlang-shell "erlang" "\
Start a new Erlang shell.\n\nThe variable `erlang-shell-function' decides which method to use,\ndefault is to start a new Erlang host.  It is possible that, in the\nfuture, a new shell on an already running host will be started.\n\n(fn)" t nil)
 (autoload 'run-erlang "erlang" "Start a new Erlang shell." t)

(autoload 'erlang-compile "erlang" "\
Compile Erlang module in current buffer.\n\n(fn)" t nil)

(autoload 'inferior-erlang "erlang" "\
Run an inferior Erlang.\nWith prefix command, prompt for command to start Erlang with.\n\nThis is just like running Erlang in a normal shell, except that\nan Emacs buffer is used for input and output.\n\\<comint-mode-map>\nThe command line history can be accessed with  \\[comint-previous-input]  and  \\[comint-next-input].\nThe history is saved between sessions.\n\nEntry to this mode calls the functions in the variables\n`comint-mode-hook' and `erlang-shell-mode-hook' with no arguments.\n\nThe following commands imitate the usual Unix interrupt and\nediting control characters:\n\\{erlang-shell-mode-map}\n\n(fn &optional COMMAND)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "erlang" '("erlang-" "inferior-erlang-" "run-erlang")))

;;;***

;;;### (autoloads nil "erlang-edoc" "erlang-edoc.el" (0 0 0 0))
;;; Generated autoloads from erlang-edoc.el

(autoload 'erlang-edoc-mode "erlang-edoc" "\
Toggle Erlang-Edoc mode on or off.\nWith a prefix argument ARG, enable Erlang-Edoc mode if ARG is\npositive, and disable it otherwise.  If called from Lisp, enable\nthe mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.\n\\{erlang-edoc-mode-map}\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "erlang-edoc" '("erlang-edoc-")))

;;;***

;;;### (autoloads nil "erlang-eunit" "erlang-eunit.el" (0 0 0 0))
;;; Generated autoloads from erlang-eunit.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "erlang-eunit" '("erlang-e" "filename-join")))

;;;***

;;;### (autoloads nil "erlang-flymake" "erlang-flymake.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from erlang-flymake.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "erlang-flymake" '("erlang-flymake-")))

;;;***

;;;### (autoloads nil "erlang-skels" "erlang-skels.el" (0 0 0 0))
;;; Generated autoloads from erlang-skels.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "erlang-skels" '("erlang-")))

;;;***

;;;### (autoloads nil "erlang-skels-old" "erlang-skels-old.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from erlang-skels-old.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "erlang-skels-old" '("erlang-")))

;;;***

;;;### (autoloads nil "erlang-start" "erlang-start.el" (0 0 0 0))
;;; Generated autoloads from erlang-start.el

(let ((a '("\\.erl\\'" . erlang-mode)) (b '("\\.hrl\\'" . erlang-mode))) (or (assoc (car a) auto-mode-alist) (setq auto-mode-alist (cons a auto-mode-alist))) (or (assoc (car b) auto-mode-alist) (setq auto-mode-alist (cons b auto-mode-alist))))

(add-to-list 'interpreter-mode-alist (cons "escript" 'erlang-mode))

(let ((erl-ext '(".jam" ".vee" ".beam"))) (while erl-ext (add-to-list 'completion-ignored-extensions (car erl-ext)) (when (boundp 'dired-omit-extensions) (add-to-list 'dired-omit-extensions (car erl-ext))) (setq erl-ext (cdr erl-ext))))

;;;***

;;;### (autoloads nil "erlang-test" "erlang-test.el" (0 0 0 0))
;;; Generated autoloads from erlang-test.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "erlang-test" '("erlang-")))

;;;***

;;;### (autoloads nil "erldoc" "erldoc.el" (0 0 0 0))
;;; Generated autoloads from erldoc.el

(autoload 'erldoc-browse "erldoc" "\
\n\n(fn MFA)" t nil)

(autoload 'erldoc-apropos "erldoc" "\
\n\n(fn PATTERN)" t nil)

(autoload 'erldoc-eldoc-function "erldoc" "\
A function suitable for `eldoc-documentation-function'.\n\n(fn)" nil nil)

(autoload 'erldoc-browse-topic "erldoc" "\
\n\n(fn TOPIC)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "erldoc" '("erldoc-")))

;;;***

;;;### (autoloads nil nil ("erlang-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; erlang-autoloads.el ends here
