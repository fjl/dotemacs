;;; company-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company" "company.el" (0 0 0 0))
;;; Generated autoloads from company.el

(autoload 'company-mode "company" "\
\"complete anything\"; is an in-buffer completion framework.\nCompletion starts automatically, depending on the values\n`company-idle-delay' and `company-minimum-prefix-length'.\n\nCompletion can be controlled with the commands:\n`company-complete-common', `company-complete-selection', `company-complete',\n`company-select-next', `company-select-previous'.  If these commands are\ncalled before `company-idle-delay', completion will also start.\n\nCompletions can be searched with `company-search-candidates' or\n`company-filter-candidates'.  These can be used while completion is\ninactive, as well.\n\nThe completion data is retrieved using `company-backends' and displayed\nusing `company-frontends'.  If you want to start a specific backend, call\nit interactively or use `company-begin-backend'.\n\nBy default, the completions list is sorted alphabetically, unless the\nbackend chooses otherwise, or `company-transformers' changes it later.\n\nregular keymap (`company-mode-map'):\n\n\\{company-mode-map}\nkeymap during active completions (`company-active-map'):\n\n\\{company-active-map}\n\n(fn &optional ARG)" t nil)

(defvar global-company-mode nil "\
Non-nil if Global Company mode is enabled.\nSee the `global-company-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `global-company-mode'.")

(custom-autoload 'global-company-mode "company" nil)

(autoload 'global-company-mode "company" "\
Toggle Company mode in all buffers.\nWith prefix ARG, enable Global Company mode if ARG is positive;\notherwise, disable it.  If called from Lisp, enable the mode if\nARG is omitted or nil.\n\nCompany mode is enabled in all buffers where\n`company-mode-on' would do it.\nSee `company-mode' for more information on Company mode.\n\n(fn &optional ARG)" t nil)

(autoload 'company-manual-begin "company" "\
\n\n(fn)" t nil)

(autoload 'company-complete "company" "\
Insert the common part of all candidates or the current selection.\nThe first time this is called, the common part is inserted, the second\ntime, or when the selection has been changed, the selected candidate is\ninserted.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company" '("company-")))

;;;***

;;;### (autoloads nil "company-abbrev" "company-abbrev.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from company-abbrev.el

(autoload 'company-abbrev "company-abbrev" "\
`company-mode' completion backend for abbrev.\n\n(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-abbrev" '("company-abbrev-insert")))

;;;***

;;;### (autoloads nil "company-bbdb" "company-bbdb.el" (0 0 0 0))
;;; Generated autoloads from company-bbdb.el

(autoload 'company-bbdb "company-bbdb" "\
`company-mode' completion backend for BBDB.\n\n(fn COMMAND &optional ARG &rest IGNORE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-bbdb" '("company-bbdb-")))

;;;***

;;;### (autoloads nil "company-capf" "company-capf.el" (0 0 0 0))
;;; Generated autoloads from company-capf.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-capf" '("company-")))

;;;***

;;;### (autoloads nil "company-clang" "company-clang.el" (0 0 0 0))
;;; Generated autoloads from company-clang.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-clang" '("company-clang")))

;;;***

;;;### (autoloads nil "company-cmake" "company-cmake.el" (0 0 0 0))
;;; Generated autoloads from company-cmake.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-cmake" '("company-cmake")))

;;;***

;;;### (autoloads nil "company-css" "company-css.el" (0 0 0 0))
;;; Generated autoloads from company-css.el

(autoload 'company-css "company-css" "\
`company-mode' completion backend for `css-mode'.\n\n(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-css" '("company-css-")))

;;;***

;;;### (autoloads nil "company-dabbrev" "company-dabbrev.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from company-dabbrev.el

(autoload 'company-dabbrev "company-dabbrev" "\
dabbrev-like `company-mode' completion backend.\n\n(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-dabbrev" '("company-dabbrev-")))

;;;***

;;;### (autoloads nil "company-dabbrev-code" "company-dabbrev-code.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from company-dabbrev-code.el

(autoload 'company-dabbrev-code "company-dabbrev-code" "\
dabbrev-like `company-mode' backend for code.\nThe backend looks for all symbols in the current buffer that aren't in\ncomments or strings.\n\n(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-dabbrev-code" '("company-dabbrev-code-")))

;;;***

;;;### (autoloads nil "company-eclim" "company-eclim.el" (0 0 0 0))
;;; Generated autoloads from company-eclim.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-eclim" '("company-eclim")))

;;;***

;;;### (autoloads nil "company-elisp" "company-elisp.el" (0 0 0 0))
;;; Generated autoloads from company-elisp.el

(autoload 'company-elisp "company-elisp" "\
`company-mode' completion backend for Emacs Lisp.\n\n(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-elisp" '("company-elisp-")))

;;;***

;;;### (autoloads nil "company-etags" "company-etags.el" (0 0 0 0))
;;; Generated autoloads from company-etags.el

(autoload 'company-etags "company-etags" "\
`company-mode' completion backend for etags.\n\n(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-etags" '("company-etags-")))

;;;***

;;;### (autoloads nil "company-files" "company-files.el" (0 0 0 0))
;;; Generated autoloads from company-files.el

(autoload 'company-files "company-files" "\
`company-mode' completion backend existing file names.\nCompletions works for proper absolute and relative files paths.\nFile paths with spaces are only supported inside strings.\n\n(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-files" '("company-file")))

;;;***

;;;### (autoloads nil "company-gtags" "company-gtags.el" (0 0 0 0))
;;; Generated autoloads from company-gtags.el

(autoload 'company-gtags "company-gtags" "\
`company-mode' completion backend for GNU Global.\n\n(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-gtags" '("company-gtags-")))

;;;***

;;;### (autoloads nil "company-ispell" "company-ispell.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from company-ispell.el

(autoload 'company-ispell "company-ispell" "\
`company-mode' completion backend using Ispell.\n\n(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-ispell" '("company-ispell-")))

;;;***

;;;### (autoloads nil "company-keywords" "company-keywords.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from company-keywords.el

(autoload 'company-keywords "company-keywords" "\
`company-mode' backend for programming language keywords.\n\n(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-keywords" '("company-keywords-")))

;;;***

;;;### (autoloads nil "company-nxml" "company-nxml.el" (0 0 0 0))
;;; Generated autoloads from company-nxml.el

(autoload 'company-nxml "company-nxml" "\
`company-mode' completion backend for `nxml-mode'.\n\n(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-nxml" '("company-nxml-")))

;;;***

;;;### (autoloads nil "company-oddmuse" "company-oddmuse.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from company-oddmuse.el

(autoload 'company-oddmuse "company-oddmuse" "\
`company-mode' completion backend for `oddmuse-mode'.\n\n(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-oddmuse" '("company-oddmuse-")))

;;;***

;;;### (autoloads nil "company-semantic" "company-semantic.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from company-semantic.el

(autoload 'company-semantic "company-semantic" "\
`company-mode' completion backend using CEDET Semantic.\n\n(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-semantic" '("company-semantic-")))

;;;***

;;;### (autoloads nil "company-template" "company-template.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from company-template.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-template" '("company-template-")))

;;;***

;;;### (autoloads nil "company-tempo" "company-tempo.el" (0 0 0 0))
;;; Generated autoloads from company-tempo.el

(autoload 'company-tempo "company-tempo" "\
`company-mode' completion backend for tempo.\n\n(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-tempo" '("company-tempo-")))

;;;***

;;;### (autoloads nil "company-tng" "company-tng.el" (0 0 0 0))
;;; Generated autoloads from company-tng.el

(autoload 'company-tng-frontend "company-tng" "\
When the user changes the selection at least once, this\nfrontend will display the candidate in the buffer as if it's\nalready there and any key outside of `company-active-map' will\nconfirm the selection and finish the completion.\n\n(fn COMMAND)" nil nil)

(autoload 'company-tng-configure-default "company-tng" "\
Applies the default configuration to enable company-tng.\n\n(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-tng" '("company-tng--")))

;;;***

;;;### (autoloads nil "company-xcode" "company-xcode.el" (0 0 0 0))
;;; Generated autoloads from company-xcode.el

(autoload 'company-xcode "company-xcode" "\
`company-mode' completion backend for Xcode projects.\n\n(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-xcode" '("company-xcode-")))

;;;***

;;;### (autoloads nil "company-yasnippet" "company-yasnippet.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from company-yasnippet.el

(autoload 'company-yasnippet "company-yasnippet" "\
`company-mode' backend for `yasnippet'.\n\nThis backend should be used with care, because as long as there are\nsnippets defined for the current major mode, this backend will always\nshadow backends that come after it.  Recommended usages:\n\n* In a buffer-local value of `company-backends', grouped with a backend or\n  several that provide actual text completions.\n\n  (add-hook 'js-mode-hook\n            (lambda ()\n              (set (make-local-variable 'company-backends)\n                   '((company-dabbrev-code company-yasnippet)))))\n\n* After keyword `:with', grouped with other backends.\n\n  (push '(company-semantic :with company-yasnippet) company-backends)\n\n* Not in `company-backends', just bound to a key.\n\n  (global-set-key (kbd \"C-c y\") 'company-yasnippet)\n\n(fn COMMAND &optional ARG &rest IGNORE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-yasnippet" '("company-yasnippet--")))

;;;***

;;;### (autoloads nil nil ("company-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-autoloads.el ends here
