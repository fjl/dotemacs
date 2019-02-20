;;; avy-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "avy" "avy.el" (0 0 0 0))
;;; Generated autoloads from avy.el

(autoload 'avy-goto-char "avy" "\
Jump to the currently visible CHAR.\nThe window scope is determined by `avy-all-windows' (ARG negates it).\n\n(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-char-in-line "avy" "\
Jump to the currently visible CHAR in the current line.\n\n(fn CHAR)" t nil)

(autoload 'avy-goto-char-2 "avy" "\
Jump to the currently visible CHAR1 followed by CHAR2.\nThe window scope is determined by `avy-all-windows'.\nWhen ARG is non-nil, do the opposite of `avy-all-windows'.\nBEG and END narrow the scope where candidates are searched.\n\n(fn CHAR1 CHAR2 &optional ARG BEG END)" t nil)

(autoload 'avy-goto-char-2-above "avy" "\
Jump to the currently visible CHAR1 followed by CHAR2.\nThis is a scoped version of `avy-goto-char-2', where the scope is\nthe visible part of the current buffer up to point.\nThe window scope is determined by `avy-all-windows'.\nWhen ARG is non-nil, do the opposite of `avy-all-windows'.\n\n(fn CHAR1 CHAR2 &optional ARG)" t nil)

(autoload 'avy-goto-char-2-below "avy" "\
Jump to the currently visible CHAR1 followed by CHAR2.\nThis is a scoped version of `avy-goto-char-2', where the scope is\nthe visible part of the current buffer following point.\nThe window scope is determined by `avy-all-windows'.\nWhen ARG is non-nil, do the opposite of `avy-all-windows'.\n\n(fn CHAR1 CHAR2 &optional ARG)" t nil)

(autoload 'avy-isearch "avy" "\
Jump to one of the current isearch candidates.\n\n(fn)" t nil)

(autoload 'avy-goto-word-0 "avy" "\
Jump to a word start.\nThe window scope is determined by `avy-all-windows'.\nWhen ARG is non-nil, do the opposite of `avy-all-windows'.\nBEG and END narrow the scope where candidates are searched.\n\n(fn ARG &optional BEG END)" t nil)

(autoload 'avy-goto-word-1 "avy" "\
Jump to the currently visible CHAR at a word start.\nThe window scope is determined by `avy-all-windows'.\nWhen ARG is non-nil, do the opposite of `avy-all-windows'.\nBEG and END narrow the scope where candidates are searched.\nWhen SYMBOL is non-nil, jump to symbol start instead of word start.\n\n(fn CHAR &optional ARG BEG END SYMBOL)" t nil)

(autoload 'avy-goto-word-1-above "avy" "\
Jump to the currently visible CHAR at a word start.\nThis is a scoped version of `avy-goto-word-1', where the scope is\nthe visible part of the current buffer up to point.\nThe window scope is determined by `avy-all-windows'.\nWhen ARG is non-nil, do the opposite of `avy-all-windows'.\n\n(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-word-1-below "avy" "\
Jump to the currently visible CHAR at a word start.\nThis is a scoped version of `avy-goto-word-1', where the scope is\nthe visible part of the current buffer following point.\nThe window scope is determined by `avy-all-windows'.\nWhen ARG is non-nil, do the opposite of `avy-all-windows'.\n\n(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-symbol-1 "avy" "\
Jump to the currently visible CHAR at a symbol start.\nThe window scope is determined by `avy-all-windows'.\nWhen ARG is non-nil, do the opposite of `avy-all-windows'.\n\n(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-symbol-1-above "avy" "\
Jump to the currently visible CHAR at a symbol start.\nThis is a scoped version of `avy-goto-symbol-1', where the scope is\nthe visible part of the current buffer up to point.\nThe window scope is determined by `avy-all-windows'.\nWhen ARG is non-nil, do the opposite of `avy-all-windows'.\n\n(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-symbol-1-below "avy" "\
Jump to the currently visible CHAR at a symbol start.\nThis is a scoped version of `avy-goto-symbol-1', where the scope is\nthe visible part of the current buffer following point.\nThe window scope is determined by `avy-all-windows'.\nWhen ARG is non-nil, do the opposite of `avy-all-windows'.\n\n(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-subword-0 "avy" "\
Jump to a word or subword start.\nThe window scope is determined by `avy-all-windows' (ARG negates it).\n\nWhen PREDICATE is non-nil it's a function of zero parameters that\nshould return true.\n\nBEG and END narrow the scope where candidates are searched.\n\n(fn &optional ARG PREDICATE BEG END)" t nil)

(autoload 'avy-goto-subword-1 "avy" "\
Jump to the currently visible CHAR at a subword start.\nThe window scope is determined by `avy-all-windows' (ARG negates it).\nThe case of CHAR is ignored.\n\n(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-word-or-subword-1 "avy" "\
Forward to `avy-goto-subword-1' or `avy-goto-word-1'.\nWhich one depends on variable `subword-mode'.\n\n(fn)" t nil)

(autoload 'avy-goto-line "avy" "\
Jump to a line start in current buffer.\n\nWhen ARG is 1, jump to lines currently visible, with the option\nto cancel to `goto-line' by entering a number.\n\nWhen ARG is 4, negate the window scope determined by\n`avy-all-windows'.\n\nOtherwise, forward to `goto-line' with ARG.\n\n(fn &optional ARG)" t nil)

(autoload 'avy-goto-line-above "avy" "\
Goto visible line above the cursor.\nOFFSET changes the distance between the closest key to the cursor and\nthe cursor\nWhen BOTTOM-UP is non-nil, display avy candidates from top to bottom\n\n(fn &optional OFFSET BOTTOM-UP)" t nil)

(autoload 'avy-goto-line-below "avy" "\
Goto visible line below the cursor.\nOFFSET changes the distance between the closest key to the cursor and\nthe cursor\nWhen BOTTOM-UP is non-nil, display avy candidates from top to bottom\n\n(fn &optional OFFSET BOTTOM-UP)" t nil)

(autoload 'avy-goto-end-of-line "avy" "\
Call `avy-goto-line' and move to the end of the line.\n\n(fn &optional ARG)" t nil)

(autoload 'avy-copy-line "avy" "\
Copy a selected line above the current line.\nARG lines can be used.\n\n(fn ARG)" t nil)

(autoload 'avy-move-line "avy" "\
Move a selected line above the current line.\nARG lines can be used.\n\n(fn ARG)" t nil)

(autoload 'avy-copy-region "avy" "\
Select two lines and copy the text between them to point.\n\nThe window scope is determined by `avy-all-windows' or\n`avy-all-windows-alt' when ARG is non-nil.\n\n(fn ARG)" t nil)

(autoload 'avy-move-region "avy" "\
Select two lines and move the text between them above the current line.\n\n(fn)" t nil)

(autoload 'avy-kill-region "avy" "\
Select two lines and kill the region between them.\n\nThe window scope is determined by `avy-all-windows' or\n`avy-all-windows-alt' when ARG is non-nil.\n\n(fn ARG)" t nil)

(autoload 'avy-kill-ring-save-region "avy" "\
Select two lines and save the region between them to the kill ring.\nThe window scope is determined by `avy-all-windows'.\nWhen ARG is non-nil, do the opposite of `avy-all-windows'.\n\n(fn ARG)" t nil)

(autoload 'avy-kill-whole-line "avy" "\
Select line and kill the whole selected line.\n\nWith a numerical prefix ARG, kill ARG line(s) starting from the\nselected line.  If ARG is negative, kill backward.\n\nIf ARG is zero, kill the selected line but exclude the trailing\nnewline.\n\n\\[universal-argument] 3 \\[avy-kil-whole-line] kill three lines\nstarting from the selected line.  \\[universal-argument] -3\n\n\\[avy-kill-whole-line] kill three lines backward including the\nselected line.\n\n(fn ARG)" t nil)

(autoload 'avy-kill-ring-save-whole-line "avy" "\
Select line and save the whole selected line as if killed, but don’t kill it.\n\nThis command is similar to `avy-kill-whole-line', except that it\nsaves the line(s) as if killed, but does not kill it(them).\n\nWith a numerical prefix ARG, kill ARG line(s) starting from the\nselected line.  If ARG is negative, kill backward.\n\nIf ARG is zero, kill the selected line but exclude the trailing\nnewline.\n\n(fn ARG)" t nil)

(autoload 'avy-setup-default "avy" "\
Setup the default shortcuts.\n\n(fn)" nil nil)

(autoload 'avy-goto-char-timer "avy" "\
Read one or many consecutive chars and jump to the first one.\nThe window scope is determined by `avy-all-windows' (ARG negates it).\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "avy" '("avy-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; avy-autoloads.el ends here
