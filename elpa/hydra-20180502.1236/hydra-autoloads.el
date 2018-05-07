;;; hydra-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "hydra" "hydra.el" (0 0 0 0))
;;; Generated autoloads from hydra.el

(autoload 'defhydra "hydra" "\
Create a Hydra - a family of functions with prefix NAME.\n\nNAME should be a symbol, it will be the prefix of all functions\ndefined here.\n\nBODY has the format:\n\n    (BODY-MAP BODY-KEY &rest BODY-PLIST)\n\nDOCSTRING will be displayed in the echo area to identify the\nHydra.  When DOCSTRING starts with a newline, special Ruby-style\nsubstitution will be performed by `hydra--format'.\n\nFunctions are created on basis of HEADS, each of which has the\nformat:\n\n    (KEY CMD &optional HINT &rest PLIST)\n\nBODY-MAP is a keymap; `global-map' is used quite often.  Each\nfunction generated from HEADS will be bound in BODY-MAP to\nBODY-KEY + KEY (both are strings passed to `kbd'), and will set\nthe transient map so that all following heads can be called\nthough KEY only.  BODY-KEY can be an empty string.\n\nCMD is a callable expression: either an interactive function\nname, or an interactive lambda, or a single sexp (it will be\nwrapped in an interactive lambda).\n\nHINT is a short string that identifies its head.  It will be\nprinted beside KEY in the echo erea if `hydra-is-helpful' is not\nnil.  If you don't even want the KEY to be printed, set HINT\nexplicitly to nil.\n\nThe heads inherit their PLIST from BODY-PLIST and are allowed to\noverride some keys.  The keys recognized are :exit and :bind.\n:exit can be:\n\n- nil (default): this head will continue the Hydra state.\n- t: this head will stop the Hydra state.\n\n:bind can be:\n- nil: this head will not be bound in BODY-MAP.\n- a lambda taking KEY and CMD used to bind a head.\n\nIt is possible to omit both BODY-MAP and BODY-KEY if you don't\nwant to bind anything.  In that case, typically you will bind the\ngenerated NAME/body command.  This command is also the return\nresult of `defhydra'.\n\n(fn NAME BODY &optional DOCSTRING &rest HEADS)" nil t)

(function-put 'defhydra 'lisp-indent-function 'defun)

(function-put 'defhydra 'doc-string-elt '3)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hydra" '("defhydradio" "hydra-")))

;;;***

;;;### (autoloads nil "hydra-examples" "hydra-examples.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from hydra-examples.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hydra-examples" '("hydra-" "org-agenda-cts" "whitespace-mode")))

;;;***

;;;### (autoloads nil "hydra-ox" "hydra-ox.el" (0 0 0 0))
;;; Generated autoloads from hydra-ox.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hydra-ox" '("hydra-ox")))

;;;***

;;;### (autoloads nil "lv" "lv.el" (0 0 0 0))
;;; Generated autoloads from lv.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lv" '("lv-")))

;;;***

;;;### (autoloads nil nil ("hydra-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hydra-autoloads.el ends here
