;;; counsel-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "counsel" "counsel.el" (0 0 0 0))
;;; Generated autoloads from counsel.el

(autoload 'counsel-el "counsel" "\
Elisp completion at point.\n\n(fn)" t nil)

(autoload 'counsel-cl "counsel" "\
Common Lisp completion at point.\n\n(fn)" t nil)

(autoload 'counsel-clj "counsel" "\
Clojure completion at point.\n\n(fn)" t nil)

(autoload 'counsel-company "counsel" "\
Complete using `company-candidates'.\n\n(fn)" t nil)

(autoload 'counsel-irony "counsel" "\
Inline C/C++ completion using Irony.\n\n(fn)" t nil)

(autoload 'counsel-describe-variable "counsel" "\
Forward to `describe-variable'.\n\nVariables declared using `defcustom' are highlighted according to\n`ivy-highlight-face'.\n\n(fn)" t nil)

(autoload 'counsel-describe-function "counsel" "\
Forward to `describe-function'.\n\nInteractive functions (i.e., commands) are highlighted according\nto `ivy-highlight-face'.\n\n(fn)" t nil)

(defface counsel-variable-documentation '((t :inherit font-lock-comment-face)) "\
Face for displaying Lisp documentation." :group (quote ivy-faces))

(autoload 'counsel-apropos "counsel" "\
Show all matching symbols.\nSee `apropos' for further information on what is considered\na symbol and how to search for them.\n\n(fn)" t nil)

(autoload 'counsel-info-lookup-symbol "counsel" "\
Forward SYMBOL to `info-lookup-symbol' with ivy completion.\nWith prefix arg MODE a query for the symbol help mode is offered.\n\n(fn SYMBOL &optional MODE)" t nil)

(autoload 'counsel-M-x "counsel" "\
Ivy version of `execute-extended-command'.\nOptional INITIAL-INPUT is the initial input in the minibuffer.\nThis function integrates with either the `amx' or `smex' package\nwhen available, in that order of precedence.\n\n(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-load-library "counsel" "\
Load a selected the Emacs Lisp library.\nThe libraries are offered from `load-path'.\n\n(fn)" t nil)

(autoload 'counsel-find-library "counsel" "\
Visit a selected the Emacs Lisp library.\nThe libraries are offered from `load-path'.\n\n(fn)" t nil)

(autoload 'counsel-load-theme "counsel" "\
Forward to `load-theme'.\nUsable with `ivy-resume', `ivy-next-line-and-call' and\n`ivy-previous-line-and-call'.\n\n(fn)" t nil)

(autoload 'counsel-descbinds "counsel" "\
Show a list of all defined keys and their definitions.\nIf non-nil, show only bindings that start with PREFIX.\nBUFFER defaults to the current one.\n\n(fn &optional PREFIX BUFFER)" t nil)

(autoload 'counsel-faces "counsel" "\
Complete faces with preview.\nActions are provided by default for describing or customizing the\nselected face.\n\n(fn)" t nil)

(autoload 'counsel-git "counsel" "\
Find file in the current Git repository.\nINITIAL-INPUT can be given as the initial minibuffer input.\n\n(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-git-grep "counsel" "\
Grep for a string in the current git repository.\nWhen CMD is a string, use it as a \"git grep\" command.\nWhen CMD is non-nil, prompt for a specific \"git grep\" command.\nINITIAL-INPUT can be given as the initial minibuffer input.\n\n(fn &optional CMD INITIAL-INPUT)" t nil)

(autoload 'counsel-git-stash "counsel" "\
Search through all available git stashes.\n\n(fn)" t nil)

(autoload 'counsel-git-change-worktree "counsel" "\
Find the file corresponding to the current buffer on a different worktree.\n\n(fn)" t nil)

(autoload 'counsel-git-checkout "counsel" "\
Call the \"git checkout\" command.\n\n(fn)" t nil)

(autoload 'counsel-git-log "counsel" "\
Call the \"git log --grep\" shell command.\n\n(fn)" t nil)

(autoload 'counsel-find-file "counsel" "\
Forward to `find-file'.\nWhen INITIAL-INPUT is non-nil, use it in the minibuffer during completion.\n\n(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-recentf "counsel" "\
Find a file on `recentf-list'.\n\n(fn)" t nil)

(autoload 'counsel-bookmark "counsel" "\
Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist.\n\n(fn)" t nil)

(autoload 'counsel-bookmarked-directory "counsel" "\
Ivy interface for bookmarked directories.\n\nWith a prefix argument, this command creates a new bookmark which points to the\ncurrent value of `default-directory'.\n\n(fn)" t nil)

(autoload 'counsel-file-register "counsel" "\
Search file in register.\n\nYou cannot use Emacs' normal register commands to create file\nregisters.  Instead you must use the `set-register' function like\nso: `(set-register ?i \"/home/eric/.emacs.d/init.el\")'.  Now you\ncan use `C-x r j i' to open that file.\n\n(fn)" t nil)

(autoload 'counsel-locate "counsel" "\
Call the \"locate\" shell command.\nINITIAL-INPUT can be given as the initial minibuffer input.\n\n(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-fzf "counsel" "\
Open a file using the fzf shell command.\nINITIAL-INPUT can be given as the initial minibuffer input.\nINITIAL-DIRECTORY, if non-nil, is used as the root directory for search.\nFZF-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.\n\n(fn &optional INITIAL-INPUT INITIAL-DIRECTORY FZF-PROMPT)" t nil)

(autoload 'counsel-dpkg "counsel" "\
Call the \"dpkg\" shell command.\n\n(fn)" t nil)

(autoload 'counsel-rpm "counsel" "\
Call the \"rpm\" shell command.\n\n(fn)" t nil)

(autoload 'counsel-file-jump "counsel" "\
Jump to a file below the current directory.\nList all files within the current directory or any of its subdirectories.\nINITIAL-INPUT can be given as the initial minibuffer input.\nINITIAL-DIRECTORY, if non-nil, is used as the root directory for search.\n\n(fn &optional INITIAL-INPUT INITIAL-DIRECTORY)" t nil)

(autoload 'counsel-dired-jump "counsel" "\
Jump to a directory (in dired) below the current directory.\nList all subdirectories within the current directory.\nINITIAL-INPUT can be given as the initial minibuffer input.\nINITIAL-DIRECTORY, if non-nil, is used as the root directory for search.\n\n(fn &optional INITIAL-INPUT INITIAL-DIRECTORY)" t nil)

(autoload 'counsel-ag "counsel" "\
Grep for a string in the current directory using ag.\nINITIAL-INPUT can be given as the initial minibuffer input.\nINITIAL-DIRECTORY, if non-nil, is used as the root directory for search.\nEXTRA-AG-ARGS string, if non-nil, is appended to `counsel-ag-base-command'.\nAG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.\n\n(fn &optional INITIAL-INPUT INITIAL-DIRECTORY EXTRA-AG-ARGS AG-PROMPT)" t nil)

(autoload 'counsel-pt "counsel" "\
Grep for a string in the current directory using pt.\nINITIAL-INPUT can be given as the initial minibuffer input.\nThis uses `counsel-ag' with `counsel-pt-base-command' instead of\n`counsel-ag-base-command'.\n\n(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-ack "counsel" "\
Grep for a string in the current directory using ack.\nINITIAL-INPUT can be given as the initial minibuffer input.\nThis uses `counsel-ag' with `counsel-ack-base-command' replacing\n`counsel-ag-base-command'.\n\n(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-rg "counsel" "\
Grep for a string in the current directory using rg.\nINITIAL-INPUT can be given as the initial minibuffer input.\nINITIAL-DIRECTORY, if non-nil, is used as the root directory for search.\nEXTRA-RG-ARGS string, if non-nil, is appended to `counsel-rg-base-command'.\nRG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.\n\n(fn &optional INITIAL-INPUT INITIAL-DIRECTORY EXTRA-RG-ARGS RG-PROMPT)" t nil)

(autoload 'counsel-grep "counsel" "\
Grep for a string in the file visited by the current buffer.\nWhen non-nil, INITIAL-INPUT is the initial search pattern.\n\n(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-grep-or-swiper "counsel" "\
Call `swiper' for small buffers and `counsel-grep' for large ones.\nWhen non-nil, INITIAL-INPUT is the initial search pattern.\n\n(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel--org-get-tags "counsel" "\
\n\n(fn)" nil nil)

(autoload 'counsel-org-tag-agenda "counsel" "\
Set tags for the current agenda item.\n\n(fn)" t nil)

(defalias 'counsel-org-goto #'counsel-outline)

(autoload 'counsel-org-goto-all "counsel" "\
Go to a different location in any org file.\n\n(fn)" t nil)

(autoload 'counsel-org-file "counsel" "\
Browse all attachments for current Org file.\n\n(fn)" t nil)

(autoload 'counsel-org-entity "counsel" "\
Complete Org entities using Ivy.\n\n(fn)" t nil)

(autoload 'counsel-org-capture "counsel" "\
Capture something.\n\n(fn)" t nil)

(autoload 'counsel-org-agenda-headlines "counsel" "\
Choose from headers of `org-mode' files in the agenda.\n\n(fn)" t nil)

(autoload 'counsel-tmm "counsel" "\
Text-mode emulation of looking and choosing from a menubar.\n\n(fn)" t nil)

(autoload 'counsel-yank-pop "counsel" "\
Ivy replacement for `yank-pop'.\nWith a plain prefix argument (\\[universal-argument]),\ntemporarily toggle the value of `counsel-yank-pop-after-point'.\nAny other value of ARG has the same meaning as in `yank-pop', but\n`counsel-yank-pop-preselect-last' determines its default value.\nSee also `counsel-yank-pop-filter' for how to filter candidates.\n\nNote: Duplicate elements of `kill-ring' are always deleted.\n\n(fn &optional ARG)" t nil)

(autoload 'counsel-imenu "counsel" "\
Jump to a buffer position indexed by imenu.\n\n(fn)" t nil)

(autoload 'counsel-list-processes "counsel" "\
Offer completion for `process-list'.\nThe default action deletes the selected process.\nAn extra action allows to switch to the process buffer.\n\n(fn)" t nil)

(autoload 'counsel-expression-history "counsel" "\
Select an element of `read-expression-history'.\nAnd insert it into the minibuffer.  Useful during `eval-expression'.\n\n(fn)" t nil)

(make-obsolete 'counsel-expression-history 'counsel-minibuffer-history '"0.10.0 <2017-11-13 Mon>")

(autoload 'counsel-shell-command-history "counsel" "\
Browse shell command history.\n\n(fn)" t nil)

(make-obsolete 'counsel-shell-command-history 'counsel-minibuffer-history '"0.10.0 <2017-11-13 Mon>")

(autoload 'counsel-minibuffer-history "counsel" "\
Browse minibuffer history.\n\n(fn)" t nil)

(autoload 'counsel-esh-history "counsel" "\
Browse Eshell history.\n\n(fn)" t nil)

(autoload 'counsel-shell-history "counsel" "\
Browse shell history.\n\n(fn)" t nil)

(autoload 'counsel-outline "counsel" "\
Jump to an outline heading with completion.\n\n(fn)" t nil)

(autoload 'counsel-ibuffer "counsel" "\
Use ibuffer to switch to another buffer.\nNAME specifies the name of the buffer (defaults to \"*Ibuffer*\").\n\n(fn &optional NAME)" t nil)

(autoload 'counsel-switch-to-shell-buffer "counsel" "\
Switch to a shell buffer, or create one.\n\n(fn)" t nil)

(autoload 'counsel-unicode-char "counsel" "\
Insert COUNT copies of a Unicode character at point.\nCOUNT defaults to 1.\n\n(fn &optional COUNT)" t nil)

(autoload 'counsel-colors-emacs "counsel" "\
Show a list of all supported colors for a particular frame.\n\nYou can insert or kill the name or hexadecimal RGB value of the\nselected color.\n\n(fn)" t nil)

(autoload 'counsel-colors-web "counsel" "\
Show a list of all W3C web colors for use in CSS.\n\nYou can insert or kill the name or hexadecimal RGB value of the\nselected color.\n\n(fn)" t nil)

(autoload 'counsel-rhythmbox "counsel" "\
Choose a song from the Rhythmbox library to play or enqueue.\n\n(fn &optional ARG)" t nil)

(autoload 'counsel-linux-app "counsel" "\
Launch a Linux desktop application, similar to Alt-<F2>.\nWhen ARG is non-nil, ignore NoDisplay property in *.desktop files.\n\n(fn &optional ARG)" t nil)

(autoload 'counsel-switch-buffer "counsel" "\
Switch to another buffer.\nDisplay a preview of the selected ivy completion candidate buffer\nin the current window.\n\n(fn)" t nil)

(defvar counsel-mode nil "\
Non-nil if Counsel mode is enabled.\nSee the `counsel-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `counsel-mode'.")

(custom-autoload 'counsel-mode "counsel" nil)

(autoload 'counsel-mode "counsel" "\
Toggle Counsel mode on or off.\nTurn Counsel mode on if ARG is positive, off otherwise. Counsel\nmode remaps built-in emacs functions that have counsel\nreplacements.\n\nLocal bindings (`counsel-mode-map'):\n\\{counsel-mode-map}\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "counsel" '("counsel-" "tmm-km-list" "ivy-function-called-at-point")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; counsel-autoloads.el ends here
