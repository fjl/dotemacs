;;; dtrt-indent-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dtrt-indent" "dtrt-indent.el" (0 0 0 0))
;;; Generated autoloads from dtrt-indent.el

(autoload 'dtrt-indent-mode "dtrt-indent" "\
Toggle dtrt-indent mode.\nWith no argument, this command toggles the mode.  Non-null prefix\nargument turns on the mode.  Null prefix argument turns off the\nmode.\n\nWhen dtrt-indent mode is enabled, the proper indentation offset\nand `indent-tabs-mode' will be guessed for newly opened files and\nadjusted transparently.\n\n(fn &optional ARG)" t nil)

(defvar dtrt-indent-global-mode nil "\
Non-nil if Dtrt-Indent-Global mode is enabled.\nSee the `dtrt-indent-global-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `dtrt-indent-global-mode'.")

(custom-autoload 'dtrt-indent-global-mode "dtrt-indent" nil)

(autoload 'dtrt-indent-global-mode "dtrt-indent" "\
Toggle Dtrt-Indent mode in all buffers.\nWith prefix ARG, enable Dtrt-Indent-Global mode if ARG is positive;\notherwise, disable it.  If called from Lisp, enable the mode if\nARG is omitted or nil.\n\nDtrt-Indent mode is enabled in all buffers where\n`(lambda nil (when (derived-mode-p (quote prog-mode) (quote text-mode)) (dtrt-indent-mode)))' would do it.\nSee `dtrt-indent-mode' for more information on Dtrt-Indent mode.\n\n(fn &optional ARG)" t nil)

(defvar dtrt-indent-mode nil "\
Toggle adaptive indentation mode.\nSetting this variable directly does not take effect;\nuse either \\[customize] or the function `dtrt-indent-mode'.")

(custom-autoload 'dtrt-indent-mode "dtrt-indent" nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dtrt-indent" '("dtrt-indent-")))

;;;***

;;;### (autoloads nil "dtrt-indent-diag" "dtrt-indent-diag.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from dtrt-indent-diag.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dtrt-indent-diag" '("dtrt-indent-" "save-buffer-state")))

;;;***

;;;### (autoloads nil nil ("dtrt-indent-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dtrt-indent-autoloads.el ends here
