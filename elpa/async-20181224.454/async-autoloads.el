;;; async-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "async" "async.el" (0 0 0 0))
;;; Generated autoloads from async.el

(autoload 'async-start-process "async" "\
Start the executable PROGRAM asynchronously.  See `async-start'.\nPROGRAM is passed PROGRAM-ARGS, calling FINISH-FUNC with the\nprocess object when done.  If FINISH-FUNC is nil, the future\nobject will return the process object when the program is\nfinished.  Set DEFAULT-DIRECTORY to change PROGRAM's current\nworking directory.\n\n(fn NAME PROGRAM FINISH-FUNC &rest PROGRAM-ARGS)" nil nil)

(autoload 'async-start "async" "\
Execute START-FUNC (often a lambda) in a subordinate Emacs process.\nWhen done, the return value is passed to FINISH-FUNC.  Example:\n\n    (async-start\n       ;; What to do in the child process\n       (lambda ()\n         (message \"This is a test\")\n         (sleep-for 3)\n         222)\n\n       ;; What to do when it finishes\n       (lambda (result)\n         (message \"Async process done, result should be 222: %s\"\n                  result)))\n\nIf FINISH-FUNC is nil or missing, a future is returned that can\nbe inspected using `async-get', blocking until the value is\nready.  Example:\n\n    (let ((proc (async-start\n                   ;; What to do in the child process\n                   (lambda ()\n                     (message \"This is a test\")\n                     (sleep-for 3)\n                     222))))\n\n        (message \"I'm going to do some work here\") ;; ....\n\n        (message \"Waiting on async process, result should be 222: %s\"\n                 (async-get proc)))\n\nIf you don't want to use a callback, and you don't care about any\nreturn value from the child process, pass the `ignore' symbol as\nthe second argument (if you don't, and never call `async-get', it\nwill leave *emacs* process buffers hanging around):\n\n    (async-start\n     (lambda ()\n       (delete-file \"a remote file on a slow link\" nil))\n     'ignore)\n\nNote: Even when FINISH-FUNC is present, a future is still\nreturned except that it yields no value (since the value is\npassed to FINISH-FUNC).  Call `async-get' on such a future always\nreturns nil.  It can still be useful, however, as an argument to\n`async-ready' or `async-wait'.\n\n(fn START-FUNC &optional FINISH-FUNC)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "async" '("async-")))

;;;***

;;;### (autoloads nil "async-bytecomp" "async-bytecomp.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from async-bytecomp.el

(autoload 'async-byte-recompile-directory "async-bytecomp" "\
Compile all *.el files in DIRECTORY asynchronously.\nAll *.elc files are systematically deleted before proceeding.\n\n(fn DIRECTORY &optional QUIET)" nil nil)

(defvar async-bytecomp-package-mode nil "\
Non-nil if Async-Bytecomp-Package mode is enabled.\nSee the `async-bytecomp-package-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `async-bytecomp-package-mode'.")

(custom-autoload 'async-bytecomp-package-mode "async-bytecomp" nil)

(autoload 'async-bytecomp-package-mode "async-bytecomp" "\
Byte compile asynchronously packages installed with package.el.\nAsync compilation of packages can be controlled by\n`async-bytecomp-allowed-packages'.\n\n(fn &optional ARG)" t nil)

(autoload 'async-byte-compile-file "async-bytecomp" "\
Byte compile Lisp code FILE asynchronously.\n\nSame as `byte-compile-file' but asynchronous.\n\n(fn FILE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "async-bytecomp" '("async-byte")))

;;;***

;;;### (autoloads nil "dired-async" "dired-async.el" (0 0 0 0))
;;; Generated autoloads from dired-async.el

(defvar dired-async-mode nil "\
Non-nil if Dired-Async mode is enabled.\nSee the `dired-async-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `dired-async-mode'.")

(custom-autoload 'dired-async-mode "dired-async" nil)

(autoload 'dired-async-mode "dired-async" "\
Do dired actions asynchronously.\n\n(fn &optional ARG)" t nil)

(autoload 'dired-async-do-copy "dired-async" "\
Run ‘dired-do-copy’ asynchronously.\n\n(fn &optional ARG)" t nil)

(autoload 'dired-async-do-symlink "dired-async" "\
Run ‘dired-do-symlink’ asynchronously.\n\n(fn &optional ARG)" t nil)

(autoload 'dired-async-do-hardlink "dired-async" "\
Run ‘dired-do-hardlink’ asynchronously.\n\n(fn &optional ARG)" t nil)

(autoload 'dired-async-do-rename "dired-async" "\
Run ‘dired-do-rename’ asynchronously.\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-async" '("dired-async-")))

;;;***

;;;### (autoloads nil "smtpmail-async" "smtpmail-async.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from smtpmail-async.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smtpmail-async" '("async-smtpmail-")))

;;;***

;;;### (autoloads nil nil ("async-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; async-autoloads.el ends here
