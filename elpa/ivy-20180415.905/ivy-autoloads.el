;;; ivy-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "colir" "colir.el" (0 0 0 0))
;;; Generated autoloads from colir.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "colir" '("colir-")))

;;;***

;;;### (autoloads nil "ivy" "ivy.el" (0 0 0 0))
;;; Generated autoloads from ivy.el

(autoload 'ivy-resume "ivy" "\
Resume the last completion session.\n\n(fn)" t nil)

(autoload 'ivy-read "ivy" "\
Read a string in the minibuffer, with completion.\n\nPROMPT is a format string, normally ending in a colon and a\nspace; %d anywhere in the string is replaced by the current\nnumber of matching candidates.  For the literal % character,\nescape it with %%. See also `ivy-count-format'.\n\nCOLLECTION is either a list of strings, a function, an alist, or\na hash table.\n\nPREDICATE is applied to filter out the COLLECTION immediately.\nThis argument is for `completing-read' compat.\n\nWhen REQUIRE-MATCH is non-nil, only members of COLLECTION can be\nselected, i.e. custom text.\n\nIf INITIAL-INPUT is not nil, then insert that input in the\nminibuffer initially.\n\nHISTORY is a name of a variable to hold the completion session\nhistory.\n\nKEYMAP is composed with `ivy-minibuffer-map'.\n\nIf PRESELECT is not nil, then select the corresponding candidate\nout of the ones that match the INITIAL-INPUT.\n\nDEF is for compatibility with `completing-read'.\n\nUPDATE-FN is called each time the current candidate(s) is changed.\n\nWhen SORT is t, use `ivy-sort-functions-alist' for sorting.\n\nACTION is a lambda function to call after selecting a result.  It\ntakes a single string argument.\n\nUNWIND is a lambda function to call before exiting.\n\nRE-BUILDER is a lambda function to call to transform text into a\nregex pattern.\n\nMATCHER is to override matching.\n\nDYNAMIC-COLLECTION is a boolean to specify if the list of\ncandidates is updated after each input by calling COLLECTION.\n\nCALLER is a symbol to uniquely identify the caller to `ivy-read'.\nIt is used, along with COLLECTION, to determine which\ncustomizations apply to the current completion session.\n\n(fn PROMPT COLLECTION &key PREDICATE REQUIRE-MATCH INITIAL-INPUT HISTORY PRESELECT DEF KEYMAP UPDATE-FN SORT ACTION UNWIND RE-BUILDER MATCHER DYNAMIC-COLLECTION CALLER)" nil nil)

(autoload 'ivy-completing-read "ivy" "\
Read a string in the minibuffer, with completion.\n\nThis interface conforms to `completing-read' and can be used for\n`completing-read-function'.\n\nPROMPT is a string that normally ends in a colon and a space.\nCOLLECTION is either a list of strings, an alist, an obarray, or a hash table.\nPREDICATE limits completion to a subset of COLLECTION.\nREQUIRE-MATCH is a boolean value.  See `completing-read'.\nINITIAL-INPUT is a string inserted into the minibuffer initially.\nHISTORY is a list of previously selected inputs.\nDEF is the default value.\nINHERIT-INPUT-METHOD is currently ignored.\n\n(fn PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HISTORY DEF INHERIT-INPUT-METHOD)" nil nil)

(defvar ivy-mode nil "\
Non-nil if Ivy mode is enabled.\nSee the `ivy-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `ivy-mode'.")

(custom-autoload 'ivy-mode "ivy" nil)

(autoload 'ivy-mode "ivy" "\
Toggle Ivy mode on or off.\nTurn Ivy mode on if ARG is positive, off otherwise.\nTurning on Ivy mode sets `completing-read-function' to\n`ivy-completing-read'.\n\nGlobal bindings:\n\\{ivy-mode-map}\n\nMinibuffer bindings:\n\\{ivy-minibuffer-map}\n\n(fn &optional ARG)" t nil)

(autoload 'ivy-switch-buffer "ivy" "\
Switch to another buffer.\n\n(fn)" t nil)

(autoload 'ivy-switch-view "ivy" "\
Switch to one of the window views stored by `ivy-push-view'.\n\n(fn)" t nil)

(autoload 'ivy-switch-buffer-other-window "ivy" "\
Switch to another buffer in another window.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy" '("ivy-" "with-ivy-window")))

;;;***

;;;### (autoloads nil "ivy-overlay" "ivy-overlay.el" (0 0 0 0))
;;; Generated autoloads from ivy-overlay.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy-overlay" '("ivy-")))

;;;***

;;;### (autoloads nil nil ("ivy-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-autoloads.el ends here
