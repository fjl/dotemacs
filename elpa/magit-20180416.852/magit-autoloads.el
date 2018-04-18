;;; magit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "git-rebase" "git-rebase.el" (0 0 0 0))
;;; Generated autoloads from git-rebase.el

(autoload 'git-rebase-mode "git-rebase" "\
Major mode for editing of a Git rebase file.\n\nRebase files are generated when you run 'git rebase -i' or run\n`magit-interactive-rebase'.  They describe how Git should perform\nthe rebase.  See the documentation for git-rebase (e.g., by\nrunning 'man git-rebase' at the command line) for details.\n\n(fn)" t nil)

(defconst git-rebase-filename-regexp "/git-rebase-todo\\'")

(add-to-list 'auto-mode-alist (cons git-rebase-filename-regexp 'git-rebase-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-rebase" '("git-rebase-")))

;;;***

;;;### (autoloads nil "magit" "magit.el" (0 0 0 0))
;;; Generated autoloads from magit.el
 (autoload 'magit-dispatch-popup "magit" nil t)
 (autoload 'magit-run-popup "magit" nil t)

(autoload 'magit-git-command "magit" "\
Execute COMMAND asynchronously; display output.\n\nInteractively, prompt for COMMAND in the minibuffer. \"git \" is\nused as initial input, but can be deleted to run another command.\n\nWith a prefix argument COMMAND is run in the top-level directory\nof the current working tree, otherwise in `default-directory'.\n\n(fn COMMAND)" t nil)

(autoload 'magit-git-command-topdir "magit" "\
Execute COMMAND asynchronously; display output.\n\nInteractively, prompt for COMMAND in the minibuffer. \"git \" is\nused as initial input, but can be deleted to run another command.\n\nCOMMAND is run in the top-level directory of the current\nworking tree.\n\n(fn COMMAND)" t nil)

(autoload 'magit-shell-command "magit" "\
Execute COMMAND asynchronously; display output.\n\nInteractively, prompt for COMMAND in the minibuffer.  With a\nprefix argument COMMAND is run in the top-level directory of\nthe current working tree, otherwise in `default-directory'.\n\n(fn COMMAND)" t nil)

(autoload 'magit-shell-command-topdir "magit" "\
Execute COMMAND asynchronously; display output.\n\nInteractively, prompt for COMMAND in the minibuffer.  COMMAND\nis run in the top-level directory of the current working tree.\n\n(fn COMMAND)" t nil)

(autoload 'magit-version "magit" "\
Return the version of Magit currently in use.\nIf optional argument PRINT-DEST is non-nil, output\nstream (interactively, the echo area, or the current buffer with\na prefix argument), also print the used versions of Magit, Git,\nand Emacs to it.\n\n(fn &optional PRINT-DEST)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit" '("magit-")))

;;;***

;;;### (autoloads nil "magit-apply" "magit-apply.el" (0 0 0 0))
;;; Generated autoloads from magit-apply.el

(autoload 'magit-stage-file "magit-apply" "\
Stage all changes to FILE.\nWith a prefix argument or when there is no file at point ask for\nthe file to be staged.  Otherwise stage the file at point without\nrequiring confirmation.\n\n(fn FILE)" t nil)

(autoload 'magit-stage-modified "magit-apply" "\
Stage all changes to files modified in the worktree.\nStage all new content of tracked files and remove tracked files\nthat no longer exist in the working tree from the index also.\nWith a prefix argument also stage previously untracked (but not\nignored) files.\n\n(fn &optional ALL)" t nil)

(autoload 'magit-unstage-file "magit-apply" "\
Unstage all changes to FILE.\nWith a prefix argument or when there is no file at point ask for\nthe file to be unstaged.  Otherwise unstage the file at point\nwithout requiring confirmation.\n\n(fn FILE)" t nil)

(autoload 'magit-unstage-all "magit-apply" "\
Remove all changes from the staging area.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-apply" '("magit-")))

;;;***

;;;### (autoloads nil "magit-autorevert" "magit-autorevert.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from magit-autorevert.el

(defvar magit-revert-buffers t)

(defvar magit-auto-revert-mode (and (with-no-warnings magit-revert-buffers) (not global-auto-revert-mode) (not noninteractive)) "\
Non-nil if Magit-Auto-Revert mode is enabled.\nSee the `magit-auto-revert-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `magit-auto-revert-mode'.")

(custom-autoload 'magit-auto-revert-mode "magit-autorevert" nil)

(autoload 'magit-auto-revert-mode "magit-autorevert" "\
Toggle Auto-Revert mode in all buffers.\nWith prefix ARG, enable Magit-Auto-Revert mode if ARG is positive;\notherwise, disable it.  If called from Lisp, enable the mode if\nARG is omitted or nil.\n\nAuto-Revert mode is enabled in all buffers where\n`magit-turn-on-auto-revert-mode-if-desired' would do it.\nSee `auto-revert-mode' for more information on Auto-Revert mode.\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-autorevert" '("auto-revert-buffer" "magit-")))

;;;***

;;;### (autoloads nil "magit-bisect" "magit-bisect.el" (0 0 0 0))
;;; Generated autoloads from magit-bisect.el
 (autoload 'magit-bisect-popup "magit-bisect" nil t)

(autoload 'magit-bisect-start "magit-bisect" "\
Start a bisect session.\n\nBisecting a bug means to find the commit that introduced it.\nThis command starts such a bisect session by asking for a know\ngood and a bad commit.  To move the session forward use the\nother actions from the bisect popup (\\<magit-status-mode-map>\\[magit-bisect-popup]).\n\n(fn BAD GOOD)" t nil)

(autoload 'magit-bisect-reset "magit-bisect" "\
After bisecting, cleanup bisection state and return to original `HEAD'.\n\n(fn)" t nil)

(autoload 'magit-bisect-good "magit-bisect" "\
While bisecting, mark the current commit as good.\nUse this after you have asserted that the commit does not contain\nthe bug in question.\n\n(fn)" t nil)

(autoload 'magit-bisect-bad "magit-bisect" "\
While bisecting, mark the current commit as bad.\nUse this after you have asserted that the commit does contain the\nbug in question.\n\n(fn)" t nil)

(autoload 'magit-bisect-skip "magit-bisect" "\
While bisecting, skip the current commit.\nUse this if for some reason the current commit is not a good one\nto test.  This command lets Git choose a different one.\n\n(fn)" t nil)

(autoload 'magit-bisect-run "magit-bisect" "\
Bisect automatically by running commands after each step.\n\nUnlike `git bisect run' this can be used before bisecting has\nbegun.  In that case it behaves like `git bisect start; git\nbisect run'.\n\n(fn CMDLINE &optional BAD GOOD)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-bisect" '("magit-")))

;;;***

;;;### (autoloads nil "magit-blame" "magit-blame.el" (0 0 0 0))
;;; Generated autoloads from magit-blame.el
 (autoload 'magit-blame-popup "magit-blame" nil t)

(autoload 'magit-blame "magit-blame" "\
For each line show the revision that last touched it.\n\n(fn)" t nil)

(autoload 'magit-blame-reverse "magit-blame" "\
For each line show the last revision in which a line still exists.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-blame" '("magit-" "auto-revert-handler--unless-magit-blame-mode")))

;;;***

;;;### (autoloads nil "magit-bookmark" "magit-bookmark.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from magit-bookmark.el

(autoload 'magit-bookmark--status-jump "magit-bookmark" "\
Handle a Magit status BOOKMARK.\n\n(fn BOOKMARK)" nil nil)

(autoload 'magit-bookmark--status-make-record "magit-bookmark" "\
Create a Magit status bookmark.\n\n(fn)" nil nil)

(autoload 'magit-bookmark--refs-jump "magit-bookmark" "\
Handle a Magit refs BOOKMARK.\n\n(fn BOOKMARK)" nil nil)

(autoload 'magit-bookmark--refs-make-record "magit-bookmark" "\
Create a Magit refs bookmark.\n\n(fn)" nil nil)

(autoload 'magit-bookmark--log-jump "magit-bookmark" "\
Handle a Magit log BOOKMARK.\n\n(fn BOOKMARK)" nil nil)

(autoload 'magit-bookmark--log-make-record "magit-bookmark" "\
Create a Magit log bookmark.\n\n(fn)" nil nil)

(autoload 'magit-bookmark--reflog-jump "magit-bookmark" "\
Handle a Magit reflog BOOKMARK.\n\n(fn BOOKMARK)" nil nil)

(autoload 'magit-bookmark--reflog-make-record "magit-bookmark" "\
Create a Magit reflog bookmark.\n\n(fn)" nil nil)

(autoload 'magit-bookmark--stashes-jump "magit-bookmark" "\
Handle a Magit stash list BOOKMARK.\n\n(fn BOOKMARK)" nil nil)

(autoload 'magit-bookmark--stashes-make-record "magit-bookmark" "\
Create a Magit stash list bookmark.\n\n(fn)" nil nil)

(autoload 'magit-bookmark--cherry-jump "magit-bookmark" "\
Handle a Magit cherry BOOKMARK.\n\n(fn BOOKMARK)" nil nil)

(autoload 'magit-bookmark--cherry-make-record "magit-bookmark" "\
Create a Magit cherry bookmark.\n\n(fn)" nil nil)

(autoload 'magit-bookmark--diff-jump "magit-bookmark" "\
Handle a Magit diff BOOKMARK.\n\n(fn BOOKMARK)" nil nil)

(autoload 'magit-bookmark--diff-make-record "magit-bookmark" "\
Create a Magit diff bookmark.\n\n(fn)" nil nil)

(autoload 'magit-bookmark--revision-jump "magit-bookmark" "\
Handle a Magit revision BOOKMARK.\n\n(fn BOOKMARK)" nil nil)

(autoload 'magit-bookmark--revision-make-record "magit-bookmark" "\
Create a Magit revision bookmark.\n\n(fn)" nil nil)

(autoload 'magit-bookmark--stash-jump "magit-bookmark" "\
Handle a Magit stash BOOKMARK.\n\n(fn BOOKMARK)" nil nil)

(autoload 'magit-bookmark--stash-make-record "magit-bookmark" "\
Create a Magit stash bookmark.\n\n(fn)" nil nil)

(autoload 'magit-bookmark--submodules-jump "magit-bookmark" "\
Handle a Magit submodule list BOOKMARK.\n\n(fn BOOKMARK)" nil nil)

(autoload 'magit-bookmark--submodules-make-record "magit-bookmark" "\
Create a Magit submodule list bookmark.\n\n(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-bookmark" '("magit-bookmark--")))

;;;***

;;;### (autoloads nil "magit-branch" "magit-branch.el" (0 0 0 0))
;;; Generated autoloads from magit-branch.el
 (autoload 'magit-branch-popup "magit" nil t)

(autoload 'magit-checkout "magit-branch" "\
Checkout REVISION, updating the index and the working tree.\nIf REVISION is a local branch, then that becomes the current\nbranch.  If it is something else, then `HEAD' becomes detached.\nCheckout fails if the working tree or the staging area contain\nchanges.\n\n(git checkout REVISION).\n\n(fn REVISION)" t nil)

(autoload 'magit-branch "magit-branch" "\
Create BRANCH at branch or revision START-POINT.\n\n(git branch [ARGS] BRANCH START-POINT).\n\n(fn BRANCH START-POINT &optional ARGS)" t nil)

(autoload 'magit-branch-and-checkout "magit-branch" "\
Create and checkout BRANCH at branch or revision START-POINT.\n\n(git checkout [ARGS] -b BRANCH START-POINT).\n\n(fn BRANCH START-POINT &optional ARGS)" t nil)

(autoload 'magit-branch-or-checkout "magit-branch" "\
Hybrid between `magit-checkout' and `magit-branch-and-checkout'.\n\nAsk the user for an existing branch or revision.  If the user\ninput actually can be resolved as a branch or revision, then\ncheck that out, just like `magit-checkout' would.\n\nOtherwise create and checkout a new branch using the input as\nits name.  Before doing so read the starting-point for the new\nbranch.  This is similar to what `magit-branch-and-checkout'\ndoes.\n\n(fn ARG &optional START-POINT)" t nil)

(autoload 'magit-branch-checkout "magit-branch" "\
Checkout an existing or new local branch.\n\nRead a branch name from the user offering all local branches and\na subset of remote branches as candidates.  Omit remote branches\nfor which a local branch by the same name exists from the list\nof candidates.  The user can also enter a completely new branch\nname.\n\n- If the user selects an existing local branch, then check that\n  out.\n\n- If the user selects a remote branch, then create and checkout\n  a new local branch with the same name.  Configure the selected\n  remote branch as push target.\n\n- If the user enters a new branch name, then create and check\n  that out, after also reading the starting-point from the user.\n\nIn the latter two cases the upstream is also set.  Whether it is\nset to the chosen START-POINT or something else depends on the\nvalue of `magit-branch-adjust-remote-upstream-alist', just like\nwhen using `magit-branch-and-checkout'.\n\n(fn BRANCH &optional START-POINT)" t nil)

(autoload 'magit-branch-orphan "magit-branch" "\
Create and checkout an orphan BRANCH with contents from revision START-POINT.\n\n(git checkout --orphan [ARGS] BRANCH START-POINT).\n\n(fn BRANCH START-POINT &optional ARGS)" t nil)

(autoload 'magit-branch-pull-request "magit-branch" "\
Create and configure a new branch from a pull-request.\nPlease see the manual for more information.\n\n(fn PR)" t nil)

(autoload 'magit-branch-spinoff "magit-branch" "\
Create new branch from the unpushed commits.\n\nCreate and checkout a new branch starting at and tracking the\ncurrent branch.  That branch in turn is reset to the last commit\nit shares with its upstream.  If the current branch has no\nupstream or no unpushed commits, then the new branch is created\nanyway and the previously current branch is not touched.\n\nThis is useful to create a feature branch after work has already\nbegan on the old branch (likely but not necessarily \"master\").\n\nIf the current branch is a member of the value of option\n`magit-branch-prefer-remote-upstream' (which see), then the\ncurrent branch will be used as the starting point as usual, but\nthe upstream of the starting-point may be used as the upstream\nof the new branch, instead of the starting-point itself.\n\nIf optional FROM is non-nil, then the source branch is reset\nto `FROM~', instead of to the last commit it shares with its\nupstream.  Interactively, FROM is only ever non-nil, if the\nregion selects some commits, and among those commits, FROM is\nthe commit that is the fewest commits ahead of the source\nbranch.\n\nThe commit at the other end of the selection actually does not\nmatter, all commits between FROM and `HEAD' are moved to the new\nbranch.  If FROM is not reachable from `HEAD' or is reachable\nfrom the source branch's upstream, then an error is raised.\n\n(fn BRANCH &optional FROM &rest ARGS)" t nil)

(autoload 'magit-branch-reset "magit-branch" "\
Reset a branch to the tip of another branch or any other commit.\n\nWhen the branch being reset is the current branch, then do a\nhard reset.  If there are any uncommitted changes, then the user\nhas to confirm the reset because those changes would be lost.\n\nThis is useful when you have started work on a feature branch but\nrealize it's all crap and want to start over.\n\nWhen resetting to another branch and a prefix argument is used,\nthen also set the target branch as the upstream of the branch\nthat is being reset.\n\n(fn BRANCH TO &optional ARGS SET-UPSTREAM)" t nil)

(autoload 'magit-branch-delete "magit-branch" "\
Delete one or multiple branches.\nIf the region marks multiple branches, then offer to delete\nthose, otherwise prompt for a single branch to be deleted,\ndefaulting to the branch at point.\n\n(fn BRANCHES &optional FORCE)" t nil)

(autoload 'magit-branch-rename "magit-branch" "\
Rename the branch named OLD to NEW.\n\nWith a prefix argument FORCE, rename even if a branch named NEW\nalready exists.\n\nIf `branch.OLD.pushRemote' is set, then unset it.  Depending on\nthe value of `magit-branch-rename-push-target' (which see) maybe\nset `branch.NEW.pushRemote' and maybe rename the push-target on\nthe remote.\n\n(fn OLD NEW &optional FORCE)" t nil)

(autoload 'magit-branch-shelve "magit-branch" "\
Shelve a BRANCH.\nRename \"refs/heads/BRANCH\" to \"refs/shelved/BRANCH\",\nand also rename the respective reflog file.\n\n(fn BRANCH)" t nil)

(autoload 'magit-branch-unshelve "magit-branch" "\
Unshelve a BRANCH\nRename \"refs/shelved/BRANCH\" to \"refs/heads/BRANCH\",\nand also rename the respective reflog file.\n\n(fn BRANCH)" t nil)

(autoload 'magit-branch-config-popup "magit-branch" "\
Popup console for setting branch variables.\n\n(fn BRANCH)" t nil)

(autoload 'magit-edit-branch*description "magit-branch" "\
Edit the description of the current branch.\nWith a prefix argument edit the description of another branch.\n\nThe description for the branch named NAME is stored in the Git\nvariable `branch.<name>.description'.\n\n(fn BRANCH)" t nil)

(autoload 'magit-set-branch*merge/remote "magit-branch" "\
Set or unset the upstream of the current branch.\nWith a prefix argument do so for another branch.\n\nWhen the branch in question already has an upstream then simply\nunsets it.  Invoke this command again to set another upstream.\n\nTogether the Git variables `branch.<name>.remote' and\n`branch.<name>.merge' define the upstream branch of the local\nbranch named NAME.  The value of `branch.<name>.remote' is the\nname of the upstream remote.  The value of `branch.<name>.merge'\nis the full reference of the upstream branch, on the remote.\n\nNon-interactively, when UPSTREAM is non-nil, then always set it\nas the new upstream, regardless of whether another upstream was\nalready set.  When nil, then always unset.\n\n(fn BRANCH UPSTREAM)" t nil)

(autoload 'magit-cycle-branch*rebase "magit-branch" "\
Cycle the value of `branch.<name>.rebase' for the current branch.\nWith a prefix argument cycle the value for another branch.\n\nThe Git variables `branch.<name>.rebase' controls whether pulling\ninto the branch named NAME is done by rebasing that branch onto\nthe fetched branch or by merging that branch.\n\nWhen `true' then pulling is done by rebasing.\nWhen `false' then pulling is done by merging.\n\nWhen that variable is undefined then the value of `pull.rebase'\nis used instead.  It defaults to `false'.\n\n(fn BRANCH)" t nil)

(autoload 'magit-cycle-branch*pushRemote "magit-branch" "\
Cycle the value of `branch.<name>.pushRemote' for the current branch.\nWith a prefix argument cycle the value for another branch.\n\nThe Git variable `branch.<name>.pushRemote' specifies the remote\nthat the branch named NAME is usually pushed to.  The value has\nto be the name of an existing remote.\n\nIf that variable is undefined, then the value of the Git variable\n`remote.pushDefault' is used instead, provided that it is defined,\nwhich by default it is not.\n\n(fn BRANCH)" t nil)

(autoload 'magit-cycle-pull\.rebase "magit-branch" "\
Cycle the repository-local value of `pull.rebase'.\n\nThe Git variable `pull.rebase' specifies whether pulling is done\nby rebasing or by merging.  It can be overwritten using the Git\nvariable `branch.<name>.rebase'.\n\nWhen `true' then pulling is done by rebasing.\nWhen `false' (the default) then pulling is done by merging.\n\n(fn)" t nil)

(autoload 'magit-cycle-remote\.pushDefault "magit-branch" "\
Cycle the repository-local value of `remote.pushDefault'.\n\nThe Git variable `remote.pushDefault' specifies the remote that\nlocal branches are usually pushed to.  It can be overwritten\nusing the Git variable `branch.<name>.pushRemote'.\n\n(fn)" t nil)

(autoload 'magit-cycle-branch*autoSetupMerge "magit-branch" "\
Cycle the repository-local value of `branch.autoSetupMerge'.\n\nThe Git variable `branch.autoSetupMerge' under what circumstances\ncreating a branch (named NAME) should result in the variables\n`branch.<name>.merge' and `branch.<name>.remote' being set\naccording to the starting point used to create the branch.  If\nthe starting point isn't a branch, then these variables are never\nset.\n\nWhen `always' then the variables are set regardless of whether\nthe starting point is a local or a remote branch.\n\nWhen `true' (the default) then the variable are set when the\nstarting point is a remote branch, but not when it is a local\nbranch.\n\nWhen `false' then the variables are never set.\n\n(fn)" t nil)

(autoload 'magit-cycle-branch*autoSetupRebase "magit-branch" "\
Cycle the repository-local value of `branch.autoSetupRebase'.\n\nThe Git variable `branch.autoSetupRebase' specifies whether\ncreating a branch (named NAME) should result in the variable\n`branch.<name>.rebase' being set to `true'.\n\nWhen `always' then the variable is set regardless of whether the\nstarting point is a local or a remote branch.\n\nWhen `local' then the variable are set when the starting point\nis a local branch, but not when it is a remote branch.\n\nWhen `remote' then the variable are set when the starting point\nis a remote branch, but not when it is a local branch.\n\nWhen `never' (the default) then the variable is never set.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-branch" '("magit-")))

;;;***

;;;### (autoloads nil "magit-collab" "magit-collab.el" (0 0 0 0))
;;; Generated autoloads from magit-collab.el

(autoload 'magit-browse-pull-request "magit-collab" "\
Visit pull-request PR using `browse-url'.\n\nCurrently this only supports Github, but that restriction will\nbe lifted eventually to support other Git forges.\n\n(fn PR)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-collab" '("magit-")))

;;;***

;;;### (autoloads nil "magit-commit" "magit-commit.el" (0 0 0 0))
;;; Generated autoloads from magit-commit.el

(autoload 'magit-commit "magit-commit" "\
Create a new commit on `HEAD'.\nWith a prefix argument, amend to the commit at `HEAD' instead.\n\n(git commit [--amend] ARGS)\n\n(fn &optional ARGS)" t nil)

(autoload 'magit-commit-amend "magit-commit" "\
Amend the last commit.\n\n(git commit --amend ARGS)\n\n(fn &optional ARGS)" t nil)

(autoload 'magit-commit-extend "magit-commit" "\
Amend the last commit, without editing the message.\n\nWith a prefix argument keep the committer date, otherwise change\nit.  The option `magit-commit-extend-override-date' can be used\nto inverse the meaning of the prefix argument.  \n(git commit\n--amend --no-edit)\n\n(fn &optional ARGS OVERRIDE-DATE)" t nil)

(autoload 'magit-commit-reword "magit-commit" "\
Reword the last commit, ignoring staged changes.\n\nWith a prefix argument keep the committer date, otherwise change\nit.  The option `magit-commit-reword-override-date' can be used\nto inverse the meaning of the prefix argument.\n\nNon-interactively respect the optional OVERRIDE-DATE argument\nand ignore the option.\n\n(git commit --amend --only)\n\n(fn &optional ARGS OVERRIDE-DATE)" t nil)

(autoload 'magit-commit-fixup "magit-commit" "\
Create a fixup commit.\n\nWith a prefix argument the target COMMIT has to be confirmed.\nOtherwise the commit at point may be used without confirmation\ndepending on the value of option `magit-commit-squash-confirm'.\n\n(fn &optional COMMIT ARGS)" t nil)

(autoload 'magit-commit-squash "magit-commit" "\
Create a squash commit, without editing the squash message.\n\nWith a prefix argument the target COMMIT has to be confirmed.\nOtherwise the commit at point may be used without confirmation\ndepending on the value of option `magit-commit-squash-confirm'.\n\n(fn &optional COMMIT ARGS)" t nil)

(autoload 'magit-commit-augment "magit-commit" "\
Create a squash commit, editing the squash message.\n\nWith a prefix argument the target COMMIT has to be confirmed.\nOtherwise the commit at point may be used without confirmation\ndepending on the value of option `magit-commit-squash-confirm'.\n\n(fn &optional COMMIT ARGS)" t nil)

(autoload 'magit-commit-instant-fixup "magit-commit" "\
Create a fixup commit targeting COMMIT and instantly rebase.\n\n(fn &optional COMMIT ARGS)" t nil)

(autoload 'magit-commit-instant-squash "magit-commit" "\
Create a squash commit targeting COMMIT and instantly rebase.\n\n(fn &optional COMMIT ARGS)" t nil)

(autoload 'magit-commit-reshelve "magit-commit" "\
Change the committer date and possibly the author date of `HEAD'.\n\nIf you are the author of `HEAD', then both dates are changed,\notherwise only the committer date.  The current time is used\nas the initial minibuffer input and the original author (if\nthat is you) or committer date is available as the previous\nhistory element.\n\n(fn DATE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-commit" '("magit-")))

;;;***

;;;### (autoloads nil "magit-diff" "magit-diff.el" (0 0 0 0))
;;; Generated autoloads from magit-diff.el

(autoload 'magit-diff-popup "magit-diff" "\
Popup console for diff commands.\n\n(fn ARG)" t nil)

(autoload 'magit-diff-buffer-file-popup "magit-diff" "\
Popup console for diff commands.\n\nThis is a variant of `magit-diff-popup' which shows the same popup\nbut which limits the diff to the file being visited in the current\nbuffer.\n\n(fn)" t nil)

(autoload 'magit-diff-dwim "magit-diff" "\
Show changes for the thing at point.\n\n(fn &optional ARGS FILES)" t nil)

(autoload 'magit-diff "magit-diff" "\
Show differences between two commits.\n\nREV-OR-RANGE should be a range or a single revision.  If it is a\nrevision, then show changes in the working tree relative to that\nrevision.  If it is a range, but one side is omitted, then show\nchanges relative to `HEAD'.\n\nIf the region is active, use the revisions on the first and last\nline of the region as the two sides of the range.  With a prefix\nargument, instead of diffing the revisions, choose a revision to\nview changes along, starting at the common ancestor of both\nrevisions (i.e., use a \"...\" range).\n\n(fn REV-OR-RANGE &optional ARGS FILES)" t nil)

(autoload 'magit-diff-working-tree "magit-diff" "\
Show changes between the current working tree and the `HEAD' commit.\nWith a prefix argument show changes between the working tree and\na commit read from the minibuffer.\n\n(fn &optional REV ARGS FILES)" t nil)

(autoload 'magit-diff-staged "magit-diff" "\
Show changes between the index and the `HEAD' commit.\nWith a prefix argument show changes between the index and\na commit read from the minibuffer.\n\n(fn &optional REV ARGS FILES)" t nil)

(autoload 'magit-diff-unstaged "magit-diff" "\
Show changes between the working tree and the index.\n\n(fn &optional ARGS FILES)" t nil)

(autoload 'magit-diff-unmerged "magit-diff" "\
Show changes that are being merged.\n\n(fn &optional ARGS FILES)" t nil)

(autoload 'magit-diff-while-committing "magit-diff" "\
While committing, show the changes that are about to be committed.\nWhile amending, invoking the command again toggles between\nshowing just the new changes or all the changes that will\nbe committed.\n\n(fn &optional ARGS)" t nil)

(autoload 'magit-diff-buffer-file "magit-diff" "\
Show diff for the blob or file visited in the current buffer.\n\n(fn)" t nil)

(autoload 'magit-diff-paths "magit-diff" "\
Show changes between any two files on disk.\n\n(fn A B)" t nil)

(autoload 'magit-show-commit "magit-diff" "\
Visit the revision at point in another buffer.\nIf there is no revision at point or with a prefix argument prompt\nfor a revision.\n\n(fn REV &optional ARGS FILES MODULE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-diff" '("magit-")))

;;;***

;;;### (autoloads nil "magit-ediff" "magit-ediff.el" (0 0 0 0))
;;; Generated autoloads from magit-ediff.el
 (autoload 'magit-ediff-popup "magit-ediff" nil t)

(autoload 'magit-ediff-resolve "magit-ediff" "\
Resolve outstanding conflicts in FILE using Ediff.\nFILE has to be relative to the top directory of the repository.\n\nIn the rare event that you want to manually resolve all\nconflicts, including those already resolved by Git, use\n`ediff-merge-revisions-with-ancestor'.\n\n(fn FILE)" t nil)

(autoload 'magit-ediff-stage "magit-ediff" "\
Stage and unstage changes to FILE using Ediff.\nFILE has to be relative to the top directory of the repository.\n\n(fn FILE)" t nil)

(autoload 'magit-ediff-compare "magit-ediff" "\
Compare REVA:FILEA with REVB:FILEB using Ediff.\n\nFILEA and FILEB have to be relative to the top directory of the\nrepository.  If REVA or REVB is nil, then this stands for the\nworking tree state.\n\nIf the region is active, use the revisions on the first and last\nline of the region.  With a prefix argument, instead of diffing\nthe revisions, choose a revision to view changes along, starting\nat the common ancestor of both revisions (i.e., use a \"...\"\nrange).\n\n(fn REVA REVB FILEA FILEB)" t nil)

(autoload 'magit-ediff-dwim "magit-ediff" "\
Compare, stage, or resolve using Ediff.\nThis command tries to guess what file, and what commit or range\nthe user wants to compare, stage, or resolve using Ediff.  It\nmight only be able to guess either the file, or range or commit,\nin which case the user is asked about the other.  It might not\nalways guess right, in which case the appropriate `magit-ediff-*'\ncommand has to be used explicitly.  If it cannot read the user's\nmind at all, then it asks the user for a command to run.\n\n(fn)" t nil)

(autoload 'magit-ediff-show-staged "magit-ediff" "\
Show staged changes using Ediff.\n\nThis only allows looking at the changes; to stage, unstage,\nand discard changes using Ediff, use `magit-ediff-stage'.\n\nFILE must be relative to the top directory of the repository.\n\n(fn FILE)" t nil)

(autoload 'magit-ediff-show-unstaged "magit-ediff" "\
Show unstaged changes using Ediff.\n\nThis only allows looking at the changes; to stage, unstage,\nand discard changes using Ediff, use `magit-ediff-stage'.\n\nFILE must be relative to the top directory of the repository.\n\n(fn FILE)" t nil)

(autoload 'magit-ediff-show-working-tree "magit-ediff" "\
Show changes between `HEAD' and working tree using Ediff.\nFILE must be relative to the top directory of the repository.\n\n(fn FILE)" t nil)

(autoload 'magit-ediff-show-commit "magit-ediff" "\
Show changes introduced by COMMIT using Ediff.\n\n(fn COMMIT)" t nil)

(autoload 'magit-ediff-show-stash "magit-ediff" "\
Show changes introduced by STASH using Ediff.\n`magit-ediff-show-stash-with-index' controls whether a\nthree-buffer Ediff is used in order to distinguish changes in the\nstash that were staged.\n\n(fn STASH)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-ediff" '("magit-ediff-")))

;;;***

;;;### (autoloads nil "magit-extras" "magit-extras.el" (0 0 0 0))
;;; Generated autoloads from magit-extras.el

(autoload 'magit-run-git-gui "magit-extras" "\
Run `git gui' for the current git repository.\n\n(fn)" t nil)

(autoload 'magit-run-git-gui-blame "magit-extras" "\
Run `git gui blame' on the given FILENAME and COMMIT.\nInteractively run it for the current file and the `HEAD', with a\nprefix or when the current file cannot be determined let the user\nchoose.  When the current buffer is visiting FILENAME instruct\nblame to center around the line point is on.\n\n(fn COMMIT FILENAME &optional LINENUM)" t nil)

(autoload 'magit-run-gitk "magit-extras" "\
Run `gitk' in the current repository.\n\n(fn)" t nil)

(autoload 'magit-run-gitk-branches "magit-extras" "\
Run `gitk --branches' in the current repository.\n\n(fn)" t nil)

(autoload 'magit-run-gitk-all "magit-extras" "\
Run `gitk --all' in the current repository.\n\n(fn)" t nil)

(autoload 'ido-enter-magit-status "magit-extras" "\
Drop into `magit-status' from file switching.\n\nTo make this command available use something like:\n\n  (add-hook \\='ido-setup-hook\n            (lambda ()\n              (define-key ido-completion-map\n                (kbd \"C-x g\") \\='ido-enter-magit-status)))\n\nStarting with Emacs 25.1 the Ido keymaps are defined just once\ninstead of every time Ido is invoked, so now you can modify it\nlike pretty much every other keymap:\n\n  (define-key ido-common-completion-map\n    (kbd \"C-x g\") 'ido-enter-magit-status)\n\n(fn)" t nil)

(autoload 'magit-dired-jump "magit-extras" "\
Visit file at point using Dired.\nWith a prefix argument, visit in another window.  If there\nis no file at point, then instead visit `default-directory'.\n\n(fn &optional OTHER-WINDOW)" t nil)

(autoload 'magit-dired-log "magit-extras" "\
Show log for all marked files, or the current file.\n\n(fn &optional FOLLOW)" t nil)

(autoload 'magit-do-async-shell-command "magit-extras" "\
Open FILE with `dired-do-async-shell-command'.\nInteractively, open the file at point.\n\n(fn FILE)" t nil)

(autoload 'magit-previous-line "magit-extras" "\
Like `previous-line' but with Magit-specific shift-selection.\n\nMagit's selection mechanism is based on the region but selects an\narea that is larger than the region.  This causes `previous-line'\nwhen invoked while holding the shift key to move up one line and\nthereby select two lines.  When invoked inside a hunk body this\ncommand does not move point on the first invocation and thereby\nit only selects a single line.  Which inconsistency you prefer\nis a matter of preference.\n\n(fn &optional ARG TRY-VSCROLL)" t nil)

(function-put 'magit-previous-line 'interactive-only '"use `forward-line' with negative argument instead.")

(autoload 'magit-next-line "magit-extras" "\
Like `next-line' but with Magit-specific shift-selection.\n\nMagit's selection mechanism is based on the region but selects\nan area that is larger than the region.  This causes `next-line'\nwhen invoked while holding the shift key to move down one line\nand thereby select two lines.  When invoked inside a hunk body\nthis command does not move point on the first invocation and\nthereby it only selects a single line.  Which inconsistency you\nprefer is a matter of preference.\n\n(fn &optional ARG TRY-VSCROLL)" t nil)

(function-put 'magit-next-line 'interactive-only 'forward-line)

(autoload 'magit-clean "magit-extras" "\
Remove untracked files from the working tree.\nWith a prefix argument also remove ignored files,\nwith two prefix arguments remove ignored files only.\n\n(git clean -f -d [-x|-X])\n\n(fn &optional ARG)" t nil)

(autoload 'magit-gitignore "magit-extras" "\
Instruct Git to ignore FILE-OR-PATTERN.\nWith a prefix argument only ignore locally.\n\n(fn FILE-OR-PATTERN &optional LOCAL)" t nil)

(autoload 'magit-gitignore-locally "magit-extras" "\
Instruct Git to locally ignore FILE-OR-PATTERN.\n\n(fn FILE-OR-PATTERN)" t nil)

(autoload 'magit-add-change-log-entry "magit-extras" "\
Find change log file and add date entry and item for current change.\nThis differs from `add-change-log-entry' (which see) in that\nit acts on the current hunk in a Magit buffer instead of on\na position in a file-visiting buffer.\n\n(fn &optional WHOAMI FILE-NAME OTHER-WINDOW)" t nil)

(autoload 'magit-add-change-log-entry-other-window "magit-extras" "\
Find change log file in other window and add entry and item.\nThis differs from `add-change-log-entry-other-window' (which see)\nin that it acts on the current hunk in a Magit buffer instead of\non a position in a file-visiting buffer.\n\n(fn &optional WHOAMI FILE-NAME)" t nil)

(autoload 'magit-reshelve-since "magit-extras" "\
Change the author and committer dates of the commits since REV.\n\nAsk the user for the first reachable commit whose dates should\nbe changed.  The read the new date for that commit.  The initial\nminibuffer input and the previous history element offer good\nvalues.  The next commit will be created one minute later and so\non.\n\nThis command is only intended for interactive use and should only\nbe used on highly rearranged and unpublished history.\n\n(fn REV)" t nil)

(autoload 'magit-copy-section-value "magit-extras" "\
Save the value of the current section for later use.\n\nSave the section value to the `kill-ring', and, provided that\nthe current section is a commit, branch, or tag section, push\nthe (referenced) revision to the `magit-revision-stack' for use\nwith `magit-pop-revision-stack'.\n\nWhen the current section is a branch or a tag, and a prefix\nargument is used, then save the revision at its tip to the\n`kill-ring' instead of the reference name.\n\nWhen the region is active, then save that to the `kill-ring',\nlike `kill-ring-save' would, instead of behaving as described\nabove.\n\n(fn)" t nil)

(autoload 'magit-copy-buffer-revision "magit-extras" "\
Save the revision of the current buffer for later use.\n\nSave the revision shown in the current buffer to the `kill-ring'\nand push it to the `magit-revision-stack'.\n\nThis command is mainly intended for use in `magit-revision-mode'\nbuffers, the only buffers where it is always unambiguous exactly\nwhich revision should be saved.\n\nMost other Magit buffers usually show more than one revision, in\nsome way or another, so this command has to select one of them,\nand that choice might not always be the one you think would have\nbeen the best pick.\n\nIn such buffers it is often more useful to save the value of\nthe current section instead, using `magit-copy-section-value'.\n\nWhen the region is active, then save that to the `kill-ring',\nlike `kill-ring-save' would, instead of behaving as described\nabove.\n\n(fn)" t nil)

(autoload 'magit-abort-dwim "magit-extras" "\
Abort current operation.\nDepending on the context, this will abort a merge, a rebase, a\npatch application, a cherry-pick, a revert, or a bisect.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-extras" '("magit-")))

;;;***

;;;### (autoloads nil "magit-files" "magit-files.el" (0 0 0 0))
;;; Generated autoloads from magit-files.el

(autoload 'magit-find-file "magit-files" "\
View FILE from REV.\nSwitch to a buffer visiting blob REV:FILE,\ncreating one if none already exists.\n\n(fn REV FILE)" t nil)

(autoload 'magit-find-file-other-window "magit-files" "\
View FILE from REV, in another window.\nLike `magit-find-file', but create a new window or reuse an\nexisting one.\n\n(fn REV FILE)" t nil)
 (autoload 'magit-file-popup "magit" nil t)

(defvar global-magit-file-mode nil "\
Non-nil if Global Magit-File mode is enabled.\nSee the `global-magit-file-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `global-magit-file-mode'.")

(custom-autoload 'global-magit-file-mode "magit-files" nil)

(autoload 'global-magit-file-mode "magit-files" "\
Toggle Magit-File mode in all buffers.\nWith prefix ARG, enable Global Magit-File mode if ARG is positive;\notherwise, disable it.  If called from Lisp, enable the mode if\nARG is omitted or nil.\n\nMagit-File mode is enabled in all buffers where\n`magit-file-mode-turn-on' would do it.\nSee `magit-file-mode' for more information on Magit-File mode.\n\n(fn &optional ARG)" t nil)

(autoload 'magit-file-checkout "magit-files" "\
Checkout FILE from REV.\n\n(fn REV FILE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-files" '("magit-")))

;;;***

;;;### (autoloads nil "magit-git" "magit-git.el" (0 0 0 0))
;;; Generated autoloads from magit-git.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-git" '("magit-")))

;;;***

;;;### (autoloads nil "magit-imenu" "magit-imenu.el" (0 0 0 0))
;;; Generated autoloads from magit-imenu.el

(autoload 'magit-imenu--log-prev-index-position-function "magit-imenu" "\
Move point to previous line in current buffer.\nThis function is used as a value for\n`imenu-prev-index-position-function'.\n\n(fn)" nil nil)

(autoload 'magit-imenu--log-extract-index-name-function "magit-imenu" "\
Return imenu name for line at point.\nThis function is used as a value for\n`imenu-extract-index-name-function'.  Point should be at the\nbeginning of the line.\n\n(fn)" nil nil)

(autoload 'magit-imenu--diff-prev-index-position-function "magit-imenu" "\
Move point to previous file line in current buffer.\nThis function is used as a value for\n`imenu-prev-index-position-function'.\n\n(fn)" nil nil)

(autoload 'magit-imenu--diff-extract-index-name-function "magit-imenu" "\
Return imenu name for line at point.\nThis function is used as a value for\n`imenu-extract-index-name-function'.  Point should be at the\nbeginning of the line.\n\n(fn)" nil nil)

(autoload 'magit-imenu--status-create-index-function "magit-imenu" "\
Return an alist of all imenu entries in current buffer.\nThis function is used as a value for\n`imenu-create-index-function'.\n\n(fn)" nil nil)

(autoload 'magit-imenu--refs-create-index-function "magit-imenu" "\
Return an alist of all imenu entries in current buffer.\nThis function is used as a value for\n`imenu-create-index-function'.\n\n(fn)" nil nil)

(autoload 'magit-imenu--cherry-create-index-function "magit-imenu" "\
Return an alist of all imenu entries in current buffer.\nThis function is used as a value for\n`imenu-create-index-function'.\n\n(fn)" nil nil)

(autoload 'magit-imenu--submodule-prev-index-position-function "magit-imenu" "\
Move point to previous line in magit-submodule-list buffer.\nThis function is used as a value for\n`imenu-prev-index-position-function'.\n\n(fn)" nil nil)

(autoload 'magit-imenu--submodule-extract-index-name-function "magit-imenu" "\
Return imenu name for line at point.\nThis function is used as a value for\n`imenu-extract-index-name-function'.  Point should be at the\nbeginning of the line.\n\n(fn)" nil nil)

(autoload 'magit-imenu--repolist-prev-index-position-function "magit-imenu" "\
Move point to previous line in magit-repolist buffer.\nThis function is used as a value for\n`imenu-prev-index-position-function'.\n\n(fn)" nil nil)

(autoload 'magit-imenu--repolist-extract-index-name-function "magit-imenu" "\
Return imenu name for line at point.\nThis function is used as a value for\n`imenu-extract-index-name-function'.  Point should be at the\nbeginning of the line.\n\n(fn)" nil nil)

(autoload 'magit-imenu--process-prev-index-position-function "magit-imenu" "\
Move point to previous process in magit-process buffer.\nThis function is used as a value for\n`imenu-prev-index-position-function'.\n\n(fn)" nil nil)

(autoload 'magit-imenu--process-extract-index-name-function "magit-imenu" "\
Return imenu name for line at point.\nThis function is used as a value for\n`imenu-extract-index-name-function'.  Point should be at the\nbeginning of the line.\n\n(fn)" nil nil)

(autoload 'magit-imenu--rebase-prev-index-position-function "magit-imenu" "\
Move point to previous commit in git-rebase buffer.\nThis function is used as a value for\n`imenu-prev-index-position-function'.\n\n(fn)" nil nil)

(autoload 'magit-imenu--rebase-extract-index-name-function "magit-imenu" "\
Return imenu name for line at point.\nThis function is used as a value for\n`imenu-extract-index-name-function'.  Point should be at the\nbeginning of the line.\n\n(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-imenu" '("magit-imenu--index-function")))

;;;***

;;;### (autoloads nil "magit-log" "magit-log.el" (0 0 0 0))
;;; Generated autoloads from magit-log.el

(autoload 'magit-log-buffer-file-popup "magit-log" "\
Popup console for log commands.\n\nThis is a variant of `magit-log-popup' which shows the same popup\nbut which limits the log to the file being visited in the current\nbuffer.\n\n(fn)" t nil)

(autoload 'magit-log-current "magit-log" "\
Show log for the current branch.\nWhen `HEAD' is detached or with a prefix argument show log for\none or more revs read from the minibuffer.\n\n(fn REVS &optional ARGS FILES)" t nil)

(autoload 'magit-log "magit-log" "\
Show log for one or more revs read from the minibuffer.\nThe user can input any revision or revisions separated by a\nspace, or even ranges, but only branches and tags, and a\nrepresentation of the commit at point, are available as\ncompletion candidates.\n\n(fn REVS &optional ARGS FILES)" t nil)

(autoload 'magit-log-head "magit-log" "\
Show log for `HEAD'.\n\n(fn &optional ARGS FILES)" t nil)

(autoload 'magit-log-branches "magit-log" "\
Show log for all local branches and `HEAD'.\n\n(fn &optional ARGS FILES)" t nil)

(autoload 'magit-log-all-branches "magit-log" "\
Show log for all local and remote branches and `HEAD'.\n\n(fn &optional ARGS FILES)" t nil)

(autoload 'magit-log-all "magit-log" "\
Show log for all references and `HEAD'.\n\n(fn &optional ARGS FILES)" t nil)

(autoload 'magit-log-buffer-file "magit-log" "\
Show log for the blob or file visited in the current buffer.\nWith a prefix argument or when `--follow' is part of\n`magit-log-arguments', then follow renames.  When the region is\nactive, restrict the log to the lines that the region touches.\n\n(fn &optional FOLLOW BEG END)" t nil)

(autoload 'magit-reflog-current "magit-log" "\
Display the reflog of the current branch.\n\n(fn)" t nil)

(autoload 'magit-reflog "magit-log" "\
Display the reflog of a branch.\n\n(fn REF)" t nil)

(autoload 'magit-reflog-head "magit-log" "\
Display the `HEAD' reflog.\n\n(fn)" t nil)

(autoload 'magit-log-move-to-parent "magit-log" "\
Move to the Nth parent of the current commit.\n\n(fn &optional N)" t nil)

(autoload 'magit-cherry "magit-log" "\
Show commits in a branch that are not merged in the upstream branch.\n\n(fn HEAD UPSTREAM)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-log" '("magit-")))

;;;***

;;;### (autoloads nil "magit-margin" "magit-margin.el" (0 0 0 0))
;;; Generated autoloads from magit-margin.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-margin" '("magit-")))

;;;***

;;;### (autoloads nil "magit-merge" "magit-merge.el" (0 0 0 0))
;;; Generated autoloads from magit-merge.el
 (autoload 'magit-merge-popup "magit" nil t)

(autoload 'magit-merge "magit-merge" "\
Merge commit REV into the current branch; using default message.\n\nUnless there are conflicts or a prefix argument is used create a\nmerge commit using a generic commit message and without letting\nthe user inspect the result.  With a prefix argument pretend the\nmerge failed to give the user the opportunity to inspect the\nmerge.\n\n(git merge --no-edit|--no-commit [ARGS] REV)\n\n(fn REV &optional ARGS NOCOMMIT)" t nil)

(autoload 'magit-merge-editmsg "magit-merge" "\
Merge commit REV into the current branch; and edit message.\nPerform the merge and prepare a commit message but let the user\nedit it.\n\n(git merge --edit --no-ff [ARGS] REV)\n\n(fn REV &optional ARGS)" t nil)

(autoload 'magit-merge-nocommit "magit-merge" "\
Merge commit REV into the current branch; pretending it failed.\nPretend the merge failed to give the user the opportunity to\ninspect the merge and change the commit message.\n\n(git merge --no-commit --no-ff [ARGS] REV)\n\n(fn REV &optional ARGS)" t nil)

(autoload 'magit-merge-into "magit-merge" "\
Merge the current branch into BRANCH and remove the former.\n\nBefore merging, force push the source branch to its push-remote,\nprovided the respective remote branch already exists, ensuring\nthat the respective pull-request (if any) won't get stuck on some\nobsolete version of the commits that are being merged.  Finally\nif `magit-branch-pull-request' was used to create the merged\nbranch, then also remove the respective remote branch.\n\n(fn BRANCH &optional ARGS)" t nil)

(autoload 'magit-merge-absorb "magit-merge" "\
Merge BRANCH into the current branch and remove the former.\n\nBefore merging, force push the source branch to its push-remote,\nprovided the respective remote branch already exists, ensuring\nthat the respective pull-request (if any) won't get stuck on some\nobsolete version of the commits that are being merged.  Finally\nif `magit-branch-pull-request' was used to create the merged\nbranch, then also remove the respective remote branch.\n\n(fn BRANCH &optional ARGS)" t nil)

(autoload 'magit-merge-squash "magit-merge" "\
Squash commit REV into the current branch; don't create a commit.\n\n(git merge --squash REV)\n\n(fn REV)" t nil)

(autoload 'magit-merge-preview "magit-merge" "\
Preview result of merging REV into the current branch.\n\n(fn REV)" t nil)

(autoload 'magit-merge-abort "magit-merge" "\
Abort the current merge operation.\n\n(git merge --abort)\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-merge" '("magit-")))

;;;***

;;;### (autoloads nil "magit-mode" "magit-mode.el" (0 0 0 0))
;;; Generated autoloads from magit-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-mode" '("magit-" "disable-magit-save-buffers" "inhibit-magit-refresh")))

;;;***

;;;### (autoloads nil "magit-notes" "magit-notes.el" (0 0 0 0))
;;; Generated autoloads from magit-notes.el
 (autoload 'magit-notes-popup "magit" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-notes" '("magit-")))

;;;***

;;;### (autoloads nil "magit-process" "magit-process.el" (0 0 0 0))
;;; Generated autoloads from magit-process.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-process" '("magit-" "tramp-sh-handle-")))

;;;***

;;;### (autoloads nil "magit-refs" "magit-refs.el" (0 0 0 0))
;;; Generated autoloads from magit-refs.el

(autoload 'magit-show-refs-popup "magit-refs" "\
Popup console for `magit-show-refs'.\n\n(fn &optional ARG)" t nil)

(autoload 'magit-show-refs-head "magit-refs" "\
List and compare references in a dedicated buffer.\nRefs are compared with `HEAD'.\n\n(fn &optional ARGS)" t nil)

(autoload 'magit-show-refs-current "magit-refs" "\
List and compare references in a dedicated buffer.\nRefs are compared with the current branch or `HEAD' if\nit is detached.\n\n(fn &optional ARGS)" t nil)

(autoload 'magit-show-refs "magit-refs" "\
List and compare references in a dedicated buffer.\nRefs are compared with a branch read from the user.\n\n(fn &optional REF ARGS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-refs" '("magit-")))

;;;***

;;;### (autoloads nil "magit-remote" "magit-remote.el" (0 0 0 0))
;;; Generated autoloads from magit-remote.el

(autoload 'magit-clone "magit-remote" "\
Clone the REPOSITORY to DIRECTORY.\nThen show the status buffer for the new repository.\n\n(fn REPOSITORY DIRECTORY)" t nil)
 (autoload 'magit-remote-popup "magit-remote" nil t)

(autoload 'magit-remote-add "magit-remote" "\
Add a remote named REMOTE and fetch it.\n\n(fn REMOTE URL &optional ARGS)" t nil)

(autoload 'magit-remote-rename "magit-remote" "\
Rename the remote named OLD to NEW.\n\n(fn OLD NEW)" t nil)

(autoload 'magit-remote-remove "magit-remote" "\
Delete the remote named REMOTE.\n\n(fn REMOTE)" t nil)

(autoload 'magit-remote-prune "magit-remote" "\
Remove stale remote-tracking branches for REMOTE.\n\n(fn REMOTE)" t nil)

(autoload 'magit-remote-prune-refspecs "magit-remote" "\
Remove stale refspecs for REMOTE.\n\nA refspec is stale if there no longer exists at least one branch\non the remote that would be fetched due to that refspec.  A stale\nrefspec is problematic because its existence causes Git to refuse\nto fetch according to the remaining non-stale refspecs.\n\nIf only stale refspecs remain, then offer to either delete the\nremote or to replace the stale refspecs with the default refspec.\n\nAlso remove the remote-tracking branches that were created due to\nthe now stale refspecs.  Other stale branches are not removed.\n\n(fn REMOTE)" t nil)

(autoload 'magit-remote-set-head "magit-remote" "\
Set the local representation of REMOTE's default branch.\nQuery REMOTE and set the symbolic-ref refs/remotes/<remote>/HEAD\naccordingly.  With a prefix argument query for the branch to be\nused, which allows you to select an incorrect value if you fancy\ndoing that.\n\n(fn REMOTE &optional BRANCH)" t nil)

(autoload 'magit-remote-unset-head "magit-remote" "\
Unset the local representation of REMOTE's default branch.\nDelete the symbolic-ref \"refs/remotes/<remote>/HEAD\".\n\n(fn REMOTE)" t nil)

(autoload 'magit-remote-config-popup "magit-remote" "\
Popup console for setting remote variables.\n\n(fn REMOTE)" t nil)
 (autoload 'magit-fetch-popup "magit-remote" nil t)

(autoload 'magit-fetch-from-pushremote "magit-remote" "\
Fetch from the push-remote of the current branch.\n\n(fn ARGS)" t nil)

(autoload 'magit-fetch-from-upstream "magit-remote" "\
Fetch from the upstream repository of the current branch.\n\n(fn ARGS)" t nil)

(autoload 'magit-fetch "magit-remote" "\
Fetch from another repository.\n\n(fn REMOTE ARGS)" t nil)

(autoload 'magit-fetch-branch "magit-remote" "\
Fetch a BRANCH from a REMOTE.\n\n(fn REMOTE BRANCH ARGS)" t nil)

(autoload 'magit-fetch-refspec "magit-remote" "\
Fetch a REFSPEC from a REMOTE.\n\n(fn REMOTE REFSPEC ARGS)" t nil)

(autoload 'magit-fetch-all "magit-remote" "\
Fetch from all remotes.\n\n(fn ARGS)" t nil)

(autoload 'magit-fetch-all-prune "magit-remote" "\
Fetch from all remotes, and prune.\nPrune remote tracking branches for branches that have been\nremoved on the respective remote.\n\n(fn)" t nil)

(autoload 'magit-fetch-all-no-prune "magit-remote" "\
Fetch from all remotes.\n\n(fn)" t nil)

(autoload 'magit-fetch-modules "magit-remote" "\
Fetch all submodules.\n\nOption `magit-fetch-modules-jobs' controls how many submodules\nare being fetched in parallel.  Also fetch the super-repository,\nbecause `git-fetch' does not support not doing that.  With a\nprefix argument fetch all remotes.\n\n(fn &optional ALL)" t nil)
 (autoload 'magit-pull-popup "magit-remote" nil t)
 (autoload 'magit-pull-and-fetch-popup "magit-remote" nil t)

(autoload 'magit-pull-from-pushremote "magit-remote" "\
Pull from the push-remote of the current branch.\n\n(fn ARGS)" t nil)

(autoload 'magit-pull-from-upstream "magit-remote" "\
Pull from the upstream of the current branch.\n\n(fn ARGS)" t nil)

(autoload 'magit-pull "magit-remote" "\
Pull from a branch read in the minibuffer.\n\n(fn SOURCE ARGS)" t nil)
 (autoload 'magit-push-popup "magit-remote" nil t)

(autoload 'magit-push-current-to-pushremote "magit-remote" "\
Push the current branch to `branch.<name>.pushRemote'.\nIf that variable is unset, then push to `remote.pushDefault'.\n\nWhen `magit-push-current-set-remote-if-missing' is non-nil and\nthe push-remote is not configured, then read the push-remote from\nthe user, set it, and then push to it.  With a prefix argument\nthe push-remote can be changed before pushed to it.\n\n(fn ARGS &optional PUSH-REMOTE)" t nil)

(autoload 'magit-push-current-to-upstream "magit-remote" "\
Push the current branch to its upstream branch.\n\nWhen `magit-push-current-set-remote-if-missing' is non-nil and\nthe upstream is not configured, then read the upstream from the\nuser, set it, and then push to it.  With a prefix argument the\nupstream can be changed before pushed to it.\n\n(fn ARGS &optional UPSTREAM)" t nil)

(autoload 'magit-push-current "magit-remote" "\
Push the current branch to a branch read in the minibuffer.\n\n(fn TARGET ARGS)" t nil)

(autoload 'magit-push "magit-remote" "\
Push an arbitrary branch or commit somewhere.\nBoth the source and the target are read in the minibuffer.\n\n(fn SOURCE TARGET ARGS)" t nil)

(autoload 'magit-push-refspecs "magit-remote" "\
Push one or multiple REFSPECS to a REMOTE.\nBoth the REMOTE and the REFSPECS are read in the minibuffer.  To\nuse multiple REFSPECS, separate them with commas.  Completion is\nonly available for the part before the colon, or when no colon\nis used.\n\n(fn REMOTE REFSPECS ARGS)" t nil)

(autoload 'magit-push-matching "magit-remote" "\
Push all matching branches to another repository.\nIf multiple remotes exist, then read one from the user.\nIf just one exists, use that without requiring confirmation.\n\n(fn REMOTE &optional ARGS)" t nil)

(autoload 'magit-push-tags "magit-remote" "\
Push all tags to another repository.\nIf only one remote exists, then push to that.  Otherwise prompt\nfor a remote, offering the remote configured for the current\nbranch as default.\n\n(fn REMOTE &optional ARGS)" t nil)

(autoload 'magit-push-tag "magit-remote" "\
Push a tag to another repository.\n\n(fn TAG REMOTE &optional ARGS)" t nil)

(autoload 'magit-push-implicitly "magit-remote" "\
Push somewhere without using an explicit refspec.\n\nThis command simply runs \"git push -v [ARGS]\".  ARGS are the\narguments specified in the popup buffer.  No explicit refspec\narguments are used.  Instead the behavior depends on at least\nthese Git variables: `push.default', `remote.pushDefault',\n`branch.<branch>.pushRemote', `branch.<branch>.remote',\n`branch.<branch>.merge', and `remote.<remote>.push'.\n\nTo add this command to the push popup add this to your init file:\n\n  (with-eval-after-load \\='magit-remote\n    (magit-define-popup-action \\='magit-push-popup ?P\n      \\='magit-push-implicitly--desc\n      \\='magit-push-implicitly ?p t))\n\nThe function `magit-push-implicitly--desc' attempts to predict\nwhat this command will do.  The value it returns is displayed in\nthe popup buffer.\n\n(fn ARGS)" t nil)

(autoload 'magit-push-to-remote "magit-remote" "\
Push to REMOTE without using an explicit refspec.\nThe REMOTE is read in the minibuffer.\n\nThis command simply runs \"git push -v [ARGS] REMOTE\".  ARGS\nare the arguments specified in the popup buffer.  No refspec\narguments are used.  Instead the behavior depends on at least\nthese Git variables: `push.default', `remote.pushDefault',\n`branch.<branch>.pushRemote', `branch.<branch>.remote',\n`branch.<branch>.merge', and `remote.<remote>.push'.\n\nTo add this command to the push popup add this to your init file:\n\n  (with-eval-after-load \\='magit-remote\n    (magit-define-popup-action \\='magit-push-popup ?r\n      \\='magit-push-to-remote--desc\n      \\='magit-push-to-remote ?p t))\n\n(fn REMOTE ARGS)" t nil)
 (autoload 'magit-patch-popup "magit-remote" nil t)

(autoload 'magit-format-patch "magit-remote" "\
Create patches for the commits in RANGE.\nWhen a single commit is given for RANGE, create a patch for the\nchanges introduced by that commit (unlike 'git format-patch'\nwhich creates patches for all commits that are reachable from\n`HEAD' but not from the specified commit).\n\n(fn RANGE ARGS)" t nil)

(autoload 'magit-request-pull "magit-remote" "\
Request upstream to pull from you public repository.\n\nURL is the url of your publically accessible repository.\nSTART is a commit that already is in the upstream repository.\nEND is the last commit, usually a branch name, which upstream\nis asked to pull.  START has to be reachable from that commit.\n\n(fn URL START END)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-remote" '("magit-")))

;;;***

;;;### (autoloads nil "magit-repos" "magit-repos.el" (0 0 0 0))
;;; Generated autoloads from magit-repos.el

(autoload 'magit-list-repositories "magit-repos" "\
Display a list of repositories.\n\nUse the options `magit-repository-directories'\nand `magit-repository-directories-depth' to\ncontrol which repositories are displayed.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-repos" '("magit-")))

;;;***

;;;### (autoloads nil "magit-reset" "magit-reset.el" (0 0 0 0))
;;; Generated autoloads from magit-reset.el
 (autoload 'magit-reset-popup "magit" nil t)

(autoload 'magit-reset-index "magit-reset" "\
Reset the index to COMMIT.\nKeep the head and working tree as-is, so if COMMIT refers to the\nhead this effectively unstages all changes.\n\n(git reset COMMIT .)\n\n(fn COMMIT)" t nil)

(autoload 'magit-reset "magit-reset" "\
Reset the head and index to COMMIT, but not the working tree.\nWith a prefix argument also reset the working tree.\n\n(git reset --mixed|--hard COMMIT)\n\n(fn COMMIT &optional HARD)" t nil)

(autoload 'magit-reset-head "magit-reset" "\
Reset the head and index to COMMIT, but not the working tree.\n\n(git reset --mixed COMMIT)\n\n(fn COMMIT)" t nil)

(autoload 'magit-reset-soft "magit-reset" "\
Reset the head to COMMIT, but not the index and working tree.\n\n(git reset --soft REVISION)\n\n(fn COMMIT)" t nil)

(autoload 'magit-reset-hard "magit-reset" "\
Reset the head, index, and working tree to COMMIT.\n\n(git reset --hard REVISION)\n\n(fn COMMIT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-reset" '("magit-reset-internal")))

;;;***

;;;### (autoloads nil "magit-section" "magit-section.el" (0 0 0 0))
;;; Generated autoloads from magit-section.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-section" '("magit-")))

;;;***

;;;### (autoloads nil "magit-sequence" "magit-sequence.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from magit-sequence.el

(autoload 'magit-sequencer-continue "magit-sequence" "\
Resume the current cherry-pick or revert sequence.\n\n(fn)" t nil)

(autoload 'magit-sequencer-skip "magit-sequence" "\
Skip the stopped at commit during a cherry-pick or revert sequence.\n\n(fn)" t nil)

(autoload 'magit-sequencer-abort "magit-sequence" "\
Abort the current cherry-pick or revert sequence.\nThis discards all changes made since the sequence started.\n\n(fn)" t nil)
 (autoload 'magit-cherry-pick-popup "magit-sequence" nil t)

(autoload 'magit-cherry-pick "magit-sequence" "\
Copy COMMITS from another branch onto the current branch.\nPrompt for a commit, defaulting to the commit at point.  If\nthe region selects multiple commits, then pick all of them,\nwithout prompting.\n\n(fn COMMITS &optional ARGS)" t nil)

(autoload 'magit-cherry-apply "magit-sequence" "\
Apply the changes in COMMITS but do not commit them.\nPrompt for a commit, defaulting to the commit at point.  If\nthe region selects multiple commits, then apply all of them,\nwithout prompting.\n\n(fn COMMITS &optional ARGS)" t nil)

(autoload 'magit-cherry-harvest "magit-sequence" "\
Move COMMITS from another BRANCH onto the current branch.\nRemove the COMMITS from BRANCH and stay on the current branch.\nIf a conflict occurs, then you have to fix that and finish the\nprocess manually.\n\n(fn COMMITS BRANCH &optional ARGS)" t nil)

(autoload 'magit-cherry-donate "magit-sequence" "\
Move COMMITS from the current branch onto another existing BRANCH.\nRemove COMMITS from the current branch and stay on that branch.\nIf a conflict occurs, then you have to fix that and finish the\nprocess manually.\n\n(fn COMMITS BRANCH &optional ARGS)" t nil)

(autoload 'magit-cherry-spinout "magit-sequence" "\
Move COMMITS from the current branch onto a new BRANCH.\nRemove COMMITS from the current branch and stay on that branch.\nIf a conflict occurs, then you have to fix that and finish the\nprocess manually.\n\n(fn COMMITS BRANCH START-POINT &optional ARGS)" t nil)

(autoload 'magit-cherry-spinoff "magit-sequence" "\
Move COMMITS from the current branch onto a new BRANCH.\nRemove COMMITS from the current branch and checkout BRANCH.\nIf a conflict occurs, then you have to fix that and finish\nthe process manually.\n\n(fn COMMITS BRANCH START-POINT &optional ARGS)" t nil)
 (autoload 'magit-revert-popup "magit-sequence" nil t)

(autoload 'magit-revert "magit-sequence" "\
Revert COMMIT by creating a new commit.\nPrompt for a commit, defaulting to the commit at point.  If\nthe region selects multiple commits, then revert all of them,\nwithout prompting.\n\n(fn COMMIT &optional ARGS)" t nil)

(autoload 'magit-revert-no-commit "magit-sequence" "\
Revert COMMIT by applying it in reverse to the worktree.\nPrompt for a commit, defaulting to the commit at point.  If\nthe region selects multiple commits, then revert all of them,\nwithout prompting.\n\n(fn COMMIT &optional ARGS)" t nil)
 (autoload 'magit-am-popup "magit-sequence" nil t)

(autoload 'magit-am-apply-patches "magit-sequence" "\
Apply the patches FILES.\n\n(fn &optional FILES ARGS)" t nil)

(autoload 'magit-am-apply-maildir "magit-sequence" "\
Apply the patches from MAILDIR.\n\n(fn &optional MAILDIR ARGS)" t nil)

(autoload 'magit-am-continue "magit-sequence" "\
Resume the current patch applying sequence.\n\n(fn)" t nil)

(autoload 'magit-am-skip "magit-sequence" "\
Skip the stopped at patch during a patch applying sequence.\n\n(fn)" t nil)

(autoload 'magit-am-abort "magit-sequence" "\
Abort the current patch applying sequence.\nThis discards all changes made since the sequence started.\n\n(fn)" t nil)
 (autoload 'magit-rebase-popup "magit-sequence" nil t)

(autoload 'magit-rebase-onto-pushremote "magit-sequence" "\
Rebase the current branch onto `branch.<name>.pushRemote'.\nIf that variable is unset, then rebase onto `remote.pushDefault'.\n\n(fn ARGS)" t nil)

(autoload 'magit-rebase-onto-upstream "magit-sequence" "\
Rebase the current branch onto its upstream branch.\n\n(fn ARGS)" t nil)

(autoload 'magit-rebase "magit-sequence" "\
Rebase the current branch onto a branch read in the minibuffer.\nAll commits that are reachable from `HEAD' but not from the\nselected branch TARGET are being rebased.\n\n(fn TARGET ARGS)" t nil)

(autoload 'magit-rebase-subset "magit-sequence" "\
Rebase a subset of the current branch's history onto a new base.\nRebase commits from START to `HEAD' onto NEWBASE.\nSTART has to be selected from a list of recent commits.\n\n(fn NEWBASE START ARGS)" t nil)

(autoload 'magit-rebase-interactive "magit-sequence" "\
Start an interactive rebase sequence.\n\n(fn COMMIT ARGS)" t nil)

(autoload 'magit-rebase-autosquash "magit-sequence" "\
Combine squash and fixup commits with their intended targets.\n\n(fn ARGS)" t nil)

(autoload 'magit-rebase-edit-commit "magit-sequence" "\
Edit a single older commit using rebase.\n\n(fn COMMIT ARGS)" t nil)

(autoload 'magit-rebase-reword-commit "magit-sequence" "\
Reword a single older commit using rebase.\n\n(fn COMMIT ARGS)" t nil)

(autoload 'magit-rebase-remove-commit "magit-sequence" "\
Remove a single older commit using rebase.\n\n(fn COMMIT ARGS)" t nil)

(autoload 'magit-rebase-continue "magit-sequence" "\
Restart the current rebasing operation.\nIn some cases this pops up a commit message buffer for you do\nedit.  With a prefix argument the old message is reused as-is.\n\n(fn &optional NOEDIT)" t nil)

(autoload 'magit-rebase-skip "magit-sequence" "\
Skip the current commit and restart the current rebase operation.\n\n(fn)" t nil)

(autoload 'magit-rebase-edit "magit-sequence" "\
Edit the todo list of the current rebase operation.\n\n(fn)" t nil)

(autoload 'magit-rebase-abort "magit-sequence" "\
Abort the current rebase operation, restoring the original branch.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-sequence" '("magit-")))

;;;***

;;;### (autoloads nil "magit-stash" "magit-stash.el" (0 0 0 0))
;;; Generated autoloads from magit-stash.el
 (autoload 'magit-stash-popup "magit-stash" nil t)

(autoload 'magit-stash "magit-stash" "\
Create a stash of the index and working tree.\nUntracked files are included according to popup arguments.\nOne prefix argument is equivalent to `--include-untracked'\nwhile two prefix arguments are equivalent to `--all'.\n\n(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-stash-index "magit-stash" "\
Create a stash of the index only.\nUnstaged and untracked changes are not stashed.  The stashed\nchanges are applied in reverse to both the index and the\nworktree.  This command can fail when the worktree is not clean.\nApplying the resulting stash has the inverse effect.\n\n(fn MESSAGE)" t nil)

(autoload 'magit-stash-worktree "magit-stash" "\
Create a stash of unstaged changes in the working tree.\nUntracked files are included according to popup arguments.\nOne prefix argument is equivalent to `--include-untracked'\nwhile two prefix arguments are equivalent to `--all'.\n\n(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-stash-keep-index "magit-stash" "\
Create a stash of the index and working tree, keeping index intact.\nUntracked files are included according to popup arguments.\nOne prefix argument is equivalent to `--include-untracked'\nwhile two prefix arguments are equivalent to `--all'.\n\n(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-snapshot "magit-stash" "\
Create a snapshot of the index and working tree.\nUntracked files are included according to popup arguments.\nOne prefix argument is equivalent to `--include-untracked'\nwhile two prefix arguments are equivalent to `--all'.\n\n(fn &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-snapshot-index "magit-stash" "\
Create a snapshot of the index only.\nUnstaged and untracked changes are not stashed.\n\n(fn)" t nil)

(autoload 'magit-snapshot-worktree "magit-stash" "\
Create a snapshot of unstaged changes in the working tree.\nUntracked files are included according to popup arguments.\nOne prefix argument is equivalent to `--include-untracked'\nwhile two prefix arguments are equivalent to `--all'.\n\n(fn &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-stash-apply "magit-stash" "\
Apply a stash to the working tree.\nTry to preserve the stash index.  If that fails because there\nare staged changes, apply without preserving the stash index.\n\n(fn STASH)" t nil)

(autoload 'magit-stash-drop "magit-stash" "\
Remove a stash from the stash list.\nWhen the region is active offer to drop all contained stashes.\n\n(fn STASH)" t nil)

(autoload 'magit-stash-clear "magit-stash" "\
Remove all stashes saved in REF's reflog by deleting REF.\n\n(fn REF)" t nil)

(autoload 'magit-stash-branch "magit-stash" "\
Create and checkout a new BRANCH from STASH.\n\n(fn STASH BRANCH)" t nil)

(autoload 'magit-stash-branch-here "magit-stash" "\
Create and checkout a new BRANCH and apply STASH.\nThe branch is created using `magit-branch', using the current\nbranch or `HEAD' as the string-point.\n\n(fn STASH BRANCH)" t nil)

(autoload 'magit-stash-format-patch "magit-stash" "\
Create a patch from STASH\n\n(fn STASH)" t nil)

(autoload 'magit-stash-list "magit-stash" "\
List all stashes in a buffer.\n\n(fn)" t nil)

(autoload 'magit-stash-show "magit-stash" "\
Show all diffs of a stash in a buffer.\n\n(fn STASH &optional ARGS FILES)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-stash" '("magit-")))

;;;***

;;;### (autoloads nil "magit-status" "magit-status.el" (0 0 0 0))
;;; Generated autoloads from magit-status.el

(autoload 'magit-init "magit-status" "\
Initialize a Git repository, then show its status.\n\nIf the directory is below an existing repository, then the user\nhas to confirm that a new one should be created inside.  If the\ndirectory is the root of the existing repository, then the user\nhas to confirm that it should be reinitialized.\n\nNon-interactively DIRECTORY is (re-)initialized unconditionally.\n\n(fn DIRECTORY)" t nil)

(autoload 'magit-status "magit-status" "\
Show the status of the current Git repository in a buffer.\nWith a prefix argument prompt for a repository to be shown.\nWith two prefix arguments prompt for an arbitrary directory.\nIf that directory isn't the root of an existing repository,\nthen offer to initialize it as a new repository.\n\n(fn &optional DIRECTORY CACHE)" t nil)

(autoload 'magit-status-internal "magit-status" "\
\n\n(fn DIRECTORY)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-status" '("magit")))

;;;***

;;;### (autoloads nil "magit-submodule" "magit-submodule.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from magit-submodule.el
 (autoload 'magit-submodule-popup "magit-submodule" nil t)

(autoload 'magit-submodule-add "magit-submodule" "\
Add the repository at URL as a module.\n\nOptional PATH is the path to the module relative to the root of\nthe superproject.  If it is nil, then the path is determined\nbased on the URL.  Optional NAME is the name of the module.  If\nit is nil, then PATH also becomes the name.\n\n(fn URL &optional PATH NAME ARGS)" t nil)

(autoload 'magit-submodule-read-name-for-path "magit-submodule" "\
\n\n(fn PATH &optional PREFER-SHORT)" nil nil)

(autoload 'magit-submodule-register "magit-submodule" "\
Register MODULES.\n\nWith a prefix argument act on all suitable modules.  Otherwise,\nif the region selects modules, then act on those.  Otherwise, if\nthere is a module at point, then act on that.  Otherwise read a\nsingle module from the user.\n\n(fn MODULES)" t nil)

(autoload 'magit-submodule-populate "magit-submodule" "\
Create MODULES working directories, checking out the recorded commits.\n\nWith a prefix argument act on all suitable modules.  Otherwise,\nif the region selects modules, then act on those.  Otherwise, if\nthere is a module at point, then act on that.  Otherwise read a\nsingle module from the user.\n\n(fn MODULES)" t nil)

(autoload 'magit-submodule-update "magit-submodule" "\
Update MODULES by checking out the recorded commits.\n\nWith a prefix argument act on all suitable modules.  Otherwise,\nif the region selects modules, then act on those.  Otherwise, if\nthere is a module at point, then act on that.  Otherwise read a\nsingle module from the user.\n\n(fn MODULES ARGS)" t nil)

(autoload 'magit-submodule-synchronize "magit-submodule" "\
Synchronize url configuration of MODULES.\n\nWith a prefix argument act on all suitable modules.  Otherwise,\nif the region selects modules, then act on those.  Otherwise, if\nthere is a module at point, then act on that.  Otherwise read a\nsingle module from the user.\n\n(fn MODULES ARGS)" t nil)

(autoload 'magit-submodule-unpopulate "magit-submodule" "\
Remove working directories of MODULES.\n\nWith a prefix argument act on all suitable modules.  Otherwise,\nif the region selects modules, then act on those.  Otherwise, if\nthere is a module at point, then act on that.  Otherwise read a\nsingle module from the user.\n\n(fn MODULES ARGS)" t nil)

(autoload 'magit-insert-modules "magit-submodule" "\
Insert submodule sections.\nHook `magit-module-sections-hook' controls which module sections\nare inserted, and option `magit-module-sections-nested' controls\nwhether they are wrapped in an additional section.\n\n(fn)" nil nil)

(autoload 'magit-insert-modules-overview "magit-submodule" "\
Insert sections for all modules.\nFor each section insert the path and the output of `git describe --tags',\nor, failing that, the abbreviated HEAD commit hash.\n\n(fn)" nil nil)

(autoload 'magit-insert-modules-unpulled-from-upstream "magit-submodule" "\
Insert sections for modules that haven't been pulled from the upstream.\nThese sections can be expanded to show the respective commits.\n\n(fn)" nil nil)

(autoload 'magit-insert-modules-unpulled-from-pushremote "magit-submodule" "\
Insert sections for modules that haven't been pulled from the push-remote.\nThese sections can be expanded to show the respective commits.\n\n(fn)" nil nil)

(autoload 'magit-insert-modules-unpushed-to-upstream "magit-submodule" "\
Insert sections for modules that haven't been pushed to the upstream.\nThese sections can be expanded to show the respective commits.\n\n(fn)" nil nil)

(autoload 'magit-insert-modules-unpushed-to-pushremote "magit-submodule" "\
Insert sections for modules that haven't been pushed to the push-remote.\nThese sections can be expanded to show the respective commits.\n\n(fn)" nil nil)

(autoload 'magit-list-submodules "magit-submodule" "\
Display a list of the current repository's submodules.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-submodule" '("magit-")))

;;;***

;;;### (autoloads nil "magit-subtree" "magit-subtree.el" (0 0 0 0))
;;; Generated autoloads from magit-subtree.el
 (autoload 'magit-subtree-popup "magit-subtree" nil t)

(autoload 'magit-subtree-add "magit-subtree" "\
Add REF from REPOSITORY as a new subtree at PREFIX.\n\n(fn PREFIX REPOSITORY REF ARGS)" t nil)

(autoload 'magit-subtree-add-commit "magit-subtree" "\
Add COMMIT as a new subtree at PREFIX.\n\n(fn PREFIX COMMIT ARGS)" t nil)

(autoload 'magit-subtree-merge "magit-subtree" "\
Merge COMMIT into the PREFIX subtree.\n\n(fn PREFIX COMMIT ARGS)" t nil)

(autoload 'magit-subtree-pull "magit-subtree" "\
Pull REF from REPOSITORY into the PREFIX subtree.\n\n(fn PREFIX REPOSITORY REF ARGS)" t nil)

(autoload 'magit-subtree-push "magit-subtree" "\
Extract the history of the subtree PREFIX and push it to REF on REPOSITORY.\n\n(fn PREFIX REPOSITORY REF ARGS)" t nil)

(autoload 'magit-subtree-split "magit-subtree" "\
Extract the history of the subtree PREFIX.\n\n(fn PREFIX COMMIT ARGS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-subtree" '("magit-")))

;;;***

;;;### (autoloads nil "magit-tag" "magit-tag.el" (0 0 0 0))
;;; Generated autoloads from magit-tag.el
 (autoload 'magit-tag-popup "magit" nil t)

(autoload 'magit-tag "magit-tag" "\
Create a new tag with the given NAME at REV.\nWith a prefix argument annotate the tag.\n\n(git tag [--annotate] NAME REV)\n\n(fn NAME REV &optional ARGS)" t nil)

(autoload 'magit-tag-delete "magit-tag" "\
Delete one or more tags.\nIf the region marks multiple tags (and nothing else), then offer\nto delete those, otherwise prompt for a single tag to be deleted,\ndefaulting to the tag at point.\n\n(git tag -d TAGS)\n\n(fn TAGS)" t nil)

(autoload 'magit-tag-prune "magit-tag" "\
Offer to delete tags missing locally from REMOTE, and vice versa.\n\n(fn TAGS REMOTE-TAGS REMOTE)" t nil)

(autoload 'magit-tag-release "magit-tag" "\
Create an opinionated release tag.\n\nAssume version tags that match \"\\\\`v?[0-9]\\\\(\\\\.[0-9]\\\\)*\\\\'\".\nPrompt for the name of the new tag using the highest existing tag\nas initial input and call \"git tag --annotate --sign -m MSG\" TAG,\nregardless of whether these arguments are enabled in the popup.\nGiven a TAG \"v1.2.3\" and a repository \"/path/to/foo-bar\", the\nMESSAGE would be \"Foo-Bar 1.2.3\".\n\nBecause it is so opinionated, this command is not available from\nthe tag popup by default.\n\n(fn TAG)" t nil)

;;;***

;;;### (autoloads nil "magit-utils" "magit-utils.el" (0 0 0 0))
;;; Generated autoloads from magit-utils.el

(autoload 'magit-emacs-Q-command "magit-utils" "\
Show a shell command that runs an uncustomized Emacs with only Magit loaded.\nSee info node `(magit)Debugging Tools' for more information.\n\n(fn)" t nil)

(autoload 'Info-follow-nearest-node--magit-gitman "magit-utils" "\
\n\n(fn FN &optional FORK)" nil nil)

(advice-add 'Info-follow-nearest-node :around 'Info-follow-nearest-node--magit-gitman)

(autoload 'org-man-export--magit-gitman "magit-utils" "\
\n\n(fn FN LINK DESCRIPTION FORMAT)" nil nil)

(advice-add 'org-man-export :around 'org-man-export--magit-gitman)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-utils" '("magit-" "whitespace-dont-turn-on-in-magit-mode")))

;;;***

;;;### (autoloads nil "magit-wip" "magit-wip.el" (0 0 0 0))
;;; Generated autoloads from magit-wip.el

(defvar magit-wip-after-save-mode nil "\
Non-nil if Magit-Wip-After-Save mode is enabled.\nSee the `magit-wip-after-save-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `magit-wip-after-save-mode'.")

(custom-autoload 'magit-wip-after-save-mode "magit-wip" nil)

(autoload 'magit-wip-after-save-mode "magit-wip" "\
Toggle Magit-Wip-After-Save-Local mode in all buffers.\nWith prefix ARG, enable Magit-Wip-After-Save mode if ARG is positive;\notherwise, disable it.  If called from Lisp, enable the mode if\nARG is omitted or nil.\n\nMagit-Wip-After-Save-Local mode is enabled in all buffers where\n`magit-wip-after-save-local-mode-turn-on' would do it.\nSee `magit-wip-after-save-local-mode' for more information on Magit-Wip-After-Save-Local mode.\n\n(fn &optional ARG)" t nil)

(defvar magit-wip-after-apply-mode nil "\
Non-nil if Magit-Wip-After-Apply mode is enabled.\nSee the `magit-wip-after-apply-mode' command\nfor a description of this minor mode.")

(custom-autoload 'magit-wip-after-apply-mode "magit-wip" nil)

(autoload 'magit-wip-after-apply-mode "magit-wip" "\
Commit to work-in-progress refs.\n\nAfter applying a change using any \"apply variant\"\ncommand (apply, stage, unstage, discard, and reverse) commit the\naffected files to the current wip refs.  For each branch there\nmay be two wip refs; one contains snapshots of the files as found\nin the worktree and the other contains snapshots of the entries\nin the index.\n\n(fn &optional ARG)" t nil)

(defvar magit-wip-before-change-mode nil "\
Non-nil if Magit-Wip-Before-Change mode is enabled.\nSee the `magit-wip-before-change-mode' command\nfor a description of this minor mode.")

(custom-autoload 'magit-wip-before-change-mode "magit-wip" nil)

(autoload 'magit-wip-before-change-mode "magit-wip" "\
Commit to work-in-progress refs before certain destructive changes.\n\nBefore invoking a revert command or an \"apply variant\"\ncommand (apply, stage, unstage, discard, and reverse) commit the\naffected tracked files to the current wip refs.  For each branch\nthere may be two wip refs; one contains snapshots of the files\nas found in the worktree and the other contains snapshots of the\nentries in the index.\n\nOnly changes to files which could potentially be affected by the\ncommand which is about to be called are committed.\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-wip" '("magit-wip-")))

;;;***

;;;### (autoloads nil "magit-worktree" "magit-worktree.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from magit-worktree.el
 (autoload 'magit-worktree-popup "magit-worktree" nil t)

(autoload 'magit-worktree-checkout "magit-worktree" "\
Checkout BRANCH in a new worktree at PATH.\n\n(fn PATH BRANCH)" t nil)

(autoload 'magit-worktree-branch "magit-worktree" "\
Create a new BRANCH and check it out in a new worktree at PATH.\n\n(fn PATH BRANCH START-POINT &optional FORCE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-worktree" '("magit-")))

;;;***

;;;### (autoloads nil nil ("magit-core.el" "magit-obsolete.el" "magit-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magit-autoloads.el ends here
