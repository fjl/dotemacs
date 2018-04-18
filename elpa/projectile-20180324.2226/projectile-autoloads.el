;;; projectile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "projectile" "projectile.el" (0 0 0 0))
;;; Generated autoloads from projectile.el

(autoload 'projectile-version "projectile" "\
Get the Projectile version as string.\n\nIf called interactively or if SHOW-VERSION is non-nil, show the\nversion in the echo area and the messages buffer.\n\nThe returned string includes both, the version from package.el\nand the library version, if both a present and different.\n\nIf the version number could not be determined, signal an error,\nif called interactively, or if SHOW-VERSION is non-nil, otherwise\njust return nil.\n\n(fn &optional SHOW-VERSION)" t nil)

(autoload 'projectile-invalidate-cache "projectile" "\
Remove the current project's files from `projectile-projects-cache'.\n\nWith a prefix argument ARG prompts for the name of the project whose cache\nto invalidate.\n\n(fn ARG)" t nil)

(autoload 'projectile-purge-file-from-cache "projectile" "\
Purge FILE from the cache of the current project.\n\n(fn FILE)" t nil)

(autoload 'projectile-purge-dir-from-cache "projectile" "\
Purge DIR from the cache of the current project.\n\n(fn DIR)" t nil)

(autoload 'projectile-cache-current-file "projectile" "\
Add the currently visited file to the cache.\n\n(fn)" t nil)

(autoload 'projectile-discover-projects-in-directory "projectile" "\
Discover any projects in DIRECTORY and add them to the projectile cache.\nThis function is not recursive and only adds projects with roots\nat the top level of DIRECTORY.\n\n(fn DIRECTORY)" t nil)

(autoload 'projectile-switch-to-buffer "projectile" "\
Switch to a project buffer.\n\n(fn)" t nil)

(autoload 'projectile-switch-to-buffer-other-window "projectile" "\
Switch to a project buffer and show it in another window.\n\n(fn)" t nil)

(autoload 'projectile-switch-to-buffer-other-frame "projectile" "\
Switch to a project buffer and show it in another window.\n\n(fn)" t nil)

(autoload 'projectile-display-buffer "projectile" "\
Display a project buffer in another window without selecting it.\n\n(fn)" t nil)

(autoload 'projectile-project-buffers-other-buffer "projectile" "\
Switch to the most recently selected buffer project buffer.\nOnly buffers not visible in windows are returned.\n\n(fn)" t nil)

(autoload 'projectile-multi-occur "projectile" "\
Do a `multi-occur' in the project's buffers.\nWith a prefix argument, show NLINES of context.\n\n(fn &optional NLINES)" t nil)

(autoload 'projectile-find-other-file "projectile" "\
Switch between files with the same name but different extensions.\nWith FLEX-MATCHING, match any file that contains the base name of current file.\nOther file extensions can be customized with the variable `projectile-other-file-alist'.\n\n(fn &optional FLEX-MATCHING)" t nil)

(autoload 'projectile-find-other-file-other-window "projectile" "\
Switch between files with the same name but different extensions in other window.\nWith FLEX-MATCHING, match any file that contains the base name of current file.\nOther file extensions can be customized with the variable `projectile-other-file-alist'.\n\n(fn &optional FLEX-MATCHING)" t nil)

(autoload 'projectile-find-other-file-other-frame "projectile" "\
Switch between files with the same name but different extensions in other window.\nWith FLEX-MATCHING, match any file that contains the base name of current file.\nOther file extensions can be customized with the variable `projectile-other-file-alist'.\n\n(fn &optional FLEX-MATCHING)" t nil)

(autoload 'projectile-find-file-dwim "projectile" "\
Jump to a project's files using completion based on context.\n\nWith a prefix ARG invalidates the cache first.\n\nIf point is on a filename, Projectile first tries to search for that\nfile in project:\n\n- If it finds just a file, it switches to that file instantly.  This works even\nif the filename is incomplete, but there's only a single file in the current project\nthat matches the filename at point.  For example, if there's only a single file named\n\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),\n`projectile-find-file-dwim' still switches to \"projectile/projectile.el\" immediately\n because this is the only filename that matches.\n\n- If it finds a list of files, the list is displayed for selecting.  A list of\nfiles is displayed when a filename appears more than one in the project or the\nfilename at point is a prefix of more than two files in a project.  For example,\nif `projectile-find-file-dwim' is executed on a filepath like \"projectile/\", it lists\nthe content of that directory.  If it is executed on a partial filename like\n \"projectile/a\", a list of files with character 'a' in that directory is presented.\n\n- If it finds nothing, display a list of all files in project for selecting.\n\n(fn &optional ARG)" t nil)

(autoload 'projectile-find-file-dwim-other-window "projectile" "\
Jump to a project's files using completion based on context in other window.\n\nWith a prefix ARG invalidates the cache first.\n\nIf point is on a filename, Projectile first tries to search for that\nfile in project:\n\n- If it finds just a file, it switches to that file instantly.  This works even\nif the filename is incomplete, but there's only a single file in the current project\nthat matches the filename at point.  For example, if there's only a single file named\n\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),\n`projectile-find-file-dwim-other-window' still switches to \"projectile/projectile.el\"\nimmediately because this is the only filename that matches.\n\n- If it finds a list of files, the list is displayed for selecting.  A list of\nfiles is displayed when a filename appears more than one in the project or the\nfilename at point is a prefix of more than two files in a project.  For example,\nif `projectile-find-file-dwim-other-window' is executed on a filepath like \"projectile/\", it lists\nthe content of that directory.  If it is executed on a partial filename\nlike \"projectile/a\", a list of files with character 'a' in that directory\nis presented.\n\n- If it finds nothing, display a list of all files in project for selecting.\n\n(fn &optional ARG)" t nil)

(autoload 'projectile-find-file-dwim-other-frame "projectile" "\
Jump to a project's files using completion based on context in other frame.\n\nWith a prefix ARG invalidates the cache first.\n\nIf point is on a filename, Projectile first tries to search for that\nfile in project:\n\n- If it finds just a file, it switches to that file instantly.  This works even\nif the filename is incomplete, but there's only a single file in the current project\nthat matches the filename at point.  For example, if there's only a single file named\n\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),\n`projectile-find-file-dwim-other-frame' still switches to \"projectile/projectile.el\"\nimmediately because this is the only filename that matches.\n\n- If it finds a list of files, the list is displayed for selecting.  A list of\nfiles is displayed when a filename appears more than one in the project or the\nfilename at point is a prefix of more than two files in a project.  For example,\nif `projectile-find-file-dwim-other-frame' is executed on a filepath like \"projectile/\", it lists\nthe content of that directory.  If it is executed on a partial filename\nlike \"projectile/a\", a list of files with character 'a' in that directory\nis presented.\n\n- If it finds nothing, display a list of all files in project for selecting.\n\n(fn &optional ARG)" t nil)

(autoload 'projectile-find-file "projectile" "\
Jump to a project's file using completion.\nWith a prefix ARG invalidates the cache first.\n\n(fn &optional ARG)" t nil)

(autoload 'projectile-find-file-other-window "projectile" "\
Jump to a project's file using completion and show it in another window.\n\nWith a prefix ARG invalidates the cache first.\n\n(fn &optional ARG)" t nil)

(autoload 'projectile-find-file-other-frame "projectile" "\
Jump to a project's file using completion and show it in another frame.\n\nWith a prefix ARG invalidates the cache first.\n\n(fn &optional ARG)" t nil)

(autoload 'projectile-find-dir "projectile" "\
Jump to a project's directory using completion.\n\nWith a prefix ARG invalidates the cache first.\n\n(fn &optional ARG)" t nil)

(autoload 'projectile-find-dir-other-window "projectile" "\
Jump to a project's directory in other window using completion.\n\nWith a prefix ARG invalidates the cache first.\n\n(fn &optional ARG)" t nil)

(autoload 'projectile-find-dir-other-frame "projectile" "\
Jump to a project's directory in other window using completion.\n\nWith a prefix ARG invalidates the cache first.\n\n(fn &optional ARG)" t nil)

(autoload 'projectile-find-test-file "projectile" "\
Jump to a project's test file using completion.\n\nWith a prefix ARG invalidates the cache first.\n\n(fn &optional ARG)" t nil)

(autoload 'projectile-project-info "projectile" "\
Display info for current project.\n\n(fn)" t nil)

(autoload 'projectile-find-implementation-or-test-other-window "projectile" "\
Open matching implementation or test file in other window.\n\n(fn)" t nil)

(autoload 'projectile-find-implementation-or-test-other-frame "projectile" "\
Open matching implementation or test file in other frame.\n\n(fn)" t nil)

(autoload 'projectile-toggle-between-implementation-and-test "projectile" "\
Toggle between an implementation file and its test file.\n\n(fn)" t nil)

(autoload 'projectile-grep "projectile" "\
Perform rgrep in the project.\n\nWith a prefix ARG asks for files (globbing-aware) which to grep in.\nWith prefix ARG of `-' (such as `M--'), default the files (without prompt),\nto `projectile-grep-default-files'.\n\nWith REGEXP given, don't query the user for a regexp.\n\n(fn &optional REGEXP ARG)" t nil)

(autoload 'projectile-ag "projectile" "\
Run an ag search with SEARCH-TERM in the project.\n\nWith an optional prefix argument ARG SEARCH-TERM is interpreted as a\nregular expression.\n\n(fn SEARCH-TERM &optional ARG)" t nil)

(autoload 'projectile-regenerate-tags "projectile" "\
Regenerate the project's [e|g]tags.\n\n(fn)" t nil)

(autoload 'projectile-find-tag "projectile" "\
Find tag in project.\n\n(fn)" t nil)

(autoload 'projectile-run-command-in-root "projectile" "\
Invoke `execute-extended-command' in the project's root.\n\n(fn)" t nil)

(autoload 'projectile-run-shell-command-in-root "projectile" "\
Invoke `shell-command' in the project's root.\n\n(fn)" t nil)

(autoload 'projectile-run-async-shell-command-in-root "projectile" "\
Invoke `async-shell-command' in the project's root.\n\n(fn)" t nil)

(autoload 'projectile-run-shell "projectile" "\
Invoke `shell' in the project's root.\n\n(fn)" t nil)

(autoload 'projectile-run-eshell "projectile" "\
Invoke `eshell' in the project's root.\n\n(fn)" t nil)

(autoload 'projectile-run-term "projectile" "\
Invoke `term' in the project's root.\n\n(fn PROGRAM)" t nil)

(autoload 'projectile-replace "projectile" "\
Replace literal string in project using non-regexp `tags-query-replace'.\n\nWith a prefix argument ARG prompts you for a directory on which\nto run the replacement.\n\n(fn &optional ARG)" t nil)

(autoload 'projectile-replace-regexp "projectile" "\
Replace a regexp in the project using `tags-query-replace'.\n\nWith a prefix argument ARG prompts you for a directory on which\nto run the replacement.\n\n(fn &optional ARG)" t nil)

(autoload 'projectile-kill-buffers "projectile" "\
Kill all project buffers.\n\n(fn)" t nil)

(autoload 'projectile-save-project-buffers "projectile" "\
Save all project buffers.\n\n(fn)" t nil)

(autoload 'projectile-dired "projectile" "\
Open `dired' at the root of the project.\n\n(fn)" t nil)

(autoload 'projectile-dired-other-window "projectile" "\
Open `dired'  at the root of the project in another window.\n\n(fn)" t nil)

(autoload 'projectile-dired-other-frame "projectile" "\
Open `dired' at the root of the project in another frame.\n\n(fn)" t nil)

(autoload 'projectile-vc "projectile" "\
Open `vc-dir' at the root of the project.\n\nFor git projects `magit-status-internal' is used if available.\nFor hg projects `monky-status' is used if available.\n\nIf PROJECT-ROOT is given, it is opened instead of the project\nroot directory of the current buffer file.  If interactively\ncalled with a prefix argument, the user is prompted for a project\ndirectory to open.\n\n(fn &optional PROJECT-ROOT)" t nil)

(autoload 'projectile-recentf "projectile" "\
Show a list of recently visited files in a project.\n\n(fn)" t nil)

(autoload 'projectile-configure-project "projectile" "\
Run project configure command.\n\nNormally you'll be prompted for a compilation command, unless\nvariable `compilation-read-command'.  You can force the prompt\nwith a prefix ARG.\n\n(fn ARG)" t nil)

(autoload 'projectile-compile-project "projectile" "\
Run project compilation command.\n\nNormally you'll be prompted for a compilation command, unless\nvariable `compilation-read-command'.  You can force the prompt\nwith a prefix ARG.\n\n(fn ARG)" t nil)

(autoload 'projectile-test-project "projectile" "\
Run project test command.\n\nNormally you'll be prompted for a compilation command, unless\nvariable `compilation-read-command'.  You can force the prompt\nwith a prefix ARG.\n\n(fn ARG)" t nil)

(autoload 'projectile-run-project "projectile" "\
Run project run command.\n\nNormally you'll be prompted for a compilation command, unless\nvariable `compilation-read-command'.  You can force the prompt\nwith a prefix ARG.\n\n(fn ARG)" t nil)

(autoload 'projectile-switch-project "projectile" "\
Switch to a project we have visited before.\nInvokes the command referenced by `projectile-switch-project-action' on switch.\nWith a prefix ARG invokes `projectile-commander' instead of\n`projectile-switch-project-action.'\n\n(fn &optional ARG)" t nil)

(autoload 'projectile-switch-open-project "projectile" "\
Switch to a project we have currently opened.\nInvokes the command referenced by `projectile-switch-project-action' on switch.\nWith a prefix ARG invokes `projectile-commander' instead of\n`projectile-switch-project-action.'\n\n(fn &optional ARG)" t nil)

(autoload 'projectile-find-file-in-directory "projectile" "\
Jump to a file in a (maybe regular) DIRECTORY.\n\nThis command will first prompt for the directory the file is in.\n\n(fn &optional DIRECTORY)" t nil)

(autoload 'projectile-find-file-in-known-projects "projectile" "\
Jump to a file in any of the known projects.\n\n(fn)" t nil)

(autoload 'projectile-cleanup-known-projects "projectile" "\
Remove known projects that don't exist anymore.\n\n(fn)" t nil)

(autoload 'projectile-clear-known-projects "projectile" "\
Clear both `projectile-known-projects' and `projectile-known-projects-file'.\n\n(fn)" t nil)

(autoload 'projectile-remove-known-project "projectile" "\
Remove PROJECT from the list of known projects.\n\n(fn &optional PROJECT)" t nil)

(autoload 'projectile-remove-current-project-from-known-projects "projectile" "\
Remove the current project from the list of known projects.\n\n(fn)" t nil)

(autoload 'projectile-ibuffer "projectile" "\
Open an IBuffer window showing all buffers in the current project.\n\nLet user choose another project when PREFIX is supplied.\n\n(fn PREFIX)" t nil)

(autoload 'projectile-commander "projectile" "\
Execute a Projectile command with a single letter.\nThe user is prompted for a single character indicating the action to invoke.\nThe `?' character describes then\navailable actions.\n\nSee `def-projectile-commander-method' for defining new methods.\n\n(fn)" t nil)

(autoload 'projectile-edit-dir-locals "projectile" "\
Edit or create a .dir-locals.el file of the project.\n\n(fn)" t nil)

(defvar projectile-mode-line '(:eval (format " Projectile[%s]" (projectile-project-name))) "\
Mode line lighter for Projectile.\n\nThe value of this variable is a mode line template as in\n`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for\ndetails about mode line templates.\n\nCustomize this variable to change how Projectile displays its\nstatus in the mode line.  The default value displays the project\nname and type.  Set this variable to nil to disable the mode line\nentirely.")

(custom-autoload 'projectile-mode-line "projectile" t)

(defvar projectile-mode nil "\
Non-nil if Projectile mode is enabled.\nSee the `projectile-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `projectile-mode'.")

(custom-autoload 'projectile-mode "projectile" nil)

(autoload 'projectile-mode "projectile" "\
Minor mode to assist project management and navigation.\n\nWhen called interactively, toggle `projectile-mode'.  With prefix\nARG, enable `projectile-mode' if ARG is positive, otherwise disable\nit.\n\nWhen called from Lisp, enable `projectile-mode' if ARG is omitted,\nnil or positive.  If ARG is `toggle', toggle `projectile-mode'.\nOtherwise behave as if called interactively.\n\n\\{projectile-mode-map}\n\n(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'projectile-global-mode 'projectile-mode)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "projectile" '("projectile-" "??" "def-projectile-commander-method")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; projectile-autoloads.el ends here
