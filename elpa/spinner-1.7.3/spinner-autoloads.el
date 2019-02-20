;;; spinner-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "spinner" "spinner.el" (0 0 0 0))
;;; Generated autoloads from spinner.el

(autoload 'spinner-create "spinner" "\
Create a spinner of the given TYPE.\nThe possible TYPEs are described in `spinner--type-to-frames'.\n\nFPS, if given, is the number of desired frames per second.\nDefault is `spinner-frames-per-second'.\n\nIf BUFFER-LOCAL is non-nil, the spinner will be automatically\ndeactivated if the buffer is killed.  If BUFFER-LOCAL is a\nbuffer, use that instead of current buffer.\n\nWhen started, in order to function properly, the spinner runs a\ntimer which periodically calls `force-mode-line-update' in the\ncurent buffer.  If BUFFER-LOCAL was set at creation time, then\n`force-mode-line-update' is called in that buffer instead.  When\nthe spinner is stopped, the timer is deactivated.\n\nDELAY, if given, is the number of seconds to wait after starting\nthe spinner before actually displaying it. It is safe to cancel\nthe spinner before this time, in which case it won't display at\nall.\n\n(fn &optional TYPE BUFFER-LOCAL FPS DELAY)" nil nil)

(autoload 'spinner-start "spinner" "\
Start a mode-line spinner of given TYPE-OR-OBJECT.\nIf TYPE-OR-OBJECT is an object created with `make-spinner',\nsimply activate it.  This method is designed for minor modes, so\nthey can use the spinner as part of their lighter by doing:\n    \\='(:eval (spinner-print THE-SPINNER))\nTo stop this spinner, call `spinner-stop' on it.\n\nIf TYPE-OR-OBJECT is anything else, a buffer-local spinner is\ncreated with this type, and it is displayed in the\n`mode-line-process' of the buffer it was created it.  Both\nTYPE-OR-OBJECT and FPS are passed to `make-spinner' (which see).\nTo stop this spinner, call `spinner-stop' in the same buffer.\n\nEither way, the return value is a function which can be called\nanywhere to stop this spinner.  You can also call `spinner-stop'\nin the same buffer where the spinner was created.\n\nFPS, if given, is the number of desired frames per second.\nDefault is `spinner-frames-per-second'.\n\nDELAY, if given, is the number of seconds to wait until actually\ndisplaying the spinner. It is safe to cancel the spinner before\nthis time, in which case it won't display at all.\n\n(fn &optional TYPE-OR-OBJECT FPS DELAY)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "spinner" '("spinner")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; spinner-autoloads.el ends here
