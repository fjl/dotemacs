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
 (autoload 'magit-dispatch "magit" nil t)
 (autoload 'magit-run "magit" nil t)

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

(defvar magit-auto-revert-mode (and (not global-auto-revert-mode) (not noninteractive)) "\
Non-nil if Magit-Auto-Revert mode is enabled.\nSee the `magit-auto-revert-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `magit-auto-revert-mode'.")

(custom-autoload 'magit-auto-revert-mode "magit-autorevert" nil)

(autoload 'magit-auto-revert-mode "magit-autorevert" "\
Toggle Auto-Revert mode in all buffers.\nWith prefix ARG, enable Magit-Auto-Revert mode if ARG is positive;\notherwise, disable it.  If called from Lisp, enable the mode if\nARG is omitted or nil.\n\nAuto-Revert mode is enabled in all buffers where\n`magit-turn-on-auto-revert-mode-if-desired' would do it.\nSee `auto-revert-mode' for more information on Auto-Revert mode.\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-autorevert" '("auto-revert-buffer" "magit-")))

;;;***

;;;### (autoloads nil "magit-bisect" "magit-bisect.el" (0 0 0 0))
;;; Generated autoloads from magit-bisect.el
 (autoload 'magit-bisect "magit-bisect" nil t)

(autoload 'magit-bisect-start "magit-bisect" "\
Start a bisect session.\n\nBisecting a bug means to find the commit that introduced it.\nThis command starts such a bisect session by asking for a know\ngood and a bad commit.  To move the session forward use the\nother actions from the bisect transient command (\\<magit-status-mode-map>\\[magit-bisect]).\n\n(fn BAD GOOD)" t nil)

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
 (autoload 'magit-blame-echo "magit-blame" nil t)
 (autoload 'magit-blame-addition "magit-blame" nil t)
 (autoload 'magit-blame-removal "magit-blame" nil t)
 (autoload 'magit-blame-reverse "magit-blame" nil t)
 (autoload 'magit-blame "magit-blame" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-blame" '("magit-")))

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
 (autoload 'magit-branch "magit" nil t)

(autoload 'magit-checkout "magit-branch" "\
Checkout REVISION, updating the index and the working tree.\nIf REVISION is a local branch, then that becomes the current\nbranch.  If it is something else, then `HEAD' becomes detached.\nCheckout fails if the working tree or the staging area contain\nchanges.\n\n(git checkout REVISION).\n\n(fn REVISION)" t nil)

(autoload 'magit-branch-create "magit-branch" "\
Create BRANCH at branch or revision START-POINT.\n\n(git branch [ARGS] BRANCH START-POINT).\n\n(fn BRANCH START-POINT &optional ARGS)" t nil)

(autoload 'magit-branch-and-checkout "magit-branch" "\
Create and checkout BRANCH at branch or revision START-POINT.\n\n(git checkout [ARGS] -b BRANCH START-POINT).\n\n(fn BRANCH START-POINT &optional ARGS)" t nil)

(autoload 'magit-branch-or-checkout "magit-branch" "\
Hybrid between `magit-checkout' and `magit-branch-and-checkout'.\n\nAsk the user for an existing branch or revision.  If the user\ninput actually can be resolved as a branch or revision, then\ncheck that out, just like `magit-checkout' would.\n\nOtherwise create and checkout a new branch using the input as\nits name.  Before doing so read the starting-point for the new\nbranch.  This is similar to what `magit-branch-and-checkout'\ndoes.\n\n(fn ARG &optional START-POINT)" t nil)

(autoload 'magit-branch-checkout "magit-branch" "\
Checkout an existing or new local branch.\n\nRead a branch name from the user offering all local branches and\na subset of remote branches as candidates.  Omit remote branches\nfor which a local branch by the same name exists from the list\nof candidates.  The user can also enter a completely new branch\nname.\n\n- If the user selects an existing local branch, then check that\n  out.\n\n- If the user selects a remote branch, then create and checkout\n  a new local branch with the same name.  Configure the selected\n  remote branch as push target.\n\n- If the user enters a new branch name, then create and check\n  that out, after also reading the starting-point from the user.\n\nIn the latter two cases the upstream is also set.  Whether it is\nset to the chosen START-POINT or something else depends on the\nvalue of `magit-branch-adjust-remote-upstream-alist', just like\nwhen using `magit-branch-and-checkout'.\n\n(fn BRANCH &optional START-POINT)" t nil)

(autoload 'magit-branch-orphan "magit-branch" "\
Create and checkout an orphan BRANCH with contents from revision START-POINT.\n\n(git checkout --orphan [ARGS] BRANCH START-POINT).\n\n(fn BRANCH START-POINT &optional ARGS)" t nil)

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
 (autoload 'magit-branch-configure "magit-branch" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-branch" '("magit-")))

;;;***

;;;### (autoloads nil "magit-clone" "magit-clone.el" (0 0 0 0))
;;; Generated autoloads from magit-clone.el

(autoload 'magit-clone "magit-clone" "\
Clone the REPOSITORY to DIRECTORY.\nThen show the status buffer for the new repository.\n\n(fn REPOSITORY DIRECTORY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-clone" '("magit-clone-")))

;;;***

;;;### (autoloads nil "magit-commit" "magit-commit.el" (0 0 0 0))
;;; Generated autoloads from magit-commit.el
 (autoload 'magit-commit "magit-commit" nil t)

(autoload 'magit-commit-create "magit-commit" "\
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
 (autoload 'magit-commit-absorb "magit-commit" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-commit" '("magit")))

;;;***

;;;### (autoloads nil "magit-diff" "magit-diff.el" (0 0 0 0))
;;; Generated autoloads from magit-diff.el

(autoload 'magit-diff-dwim "magit-diff" "\
Show changes for the thing at point.\n\n(fn &optional ARGS FILES)" t nil)

(autoload 'magit-diff-range "magit-diff" "\
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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-diff" '("magit")))

;;;***

;;;### (autoloads nil "magit-ediff" "magit-ediff.el" (0 0 0 0))
;;; Generated autoloads from magit-ediff.el
 (autoload 'magit-ediff "magit-ediff" nil)

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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-ediff" '("magit-ediff")))

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
Drop into `magit-status' from file switching.\n\nThis command does not work in Emacs 26.  It does work in Emacs 25\nand Emacs 27.  See https://github.com/magit/magit/issues/3634 and\nhttps://debbugs.gnu.org/cgi/bugreport.cgi?bug=31707.\n\nTo make this command available use something like:\n\n  (add-hook \\='ido-setup-hook\n            (lambda ()\n              (define-key ido-completion-map\n                (kbd \"C-x g\") \\='ido-enter-magit-status)))\n\nStarting with Emacs 25.1 the Ido keymaps are defined just once\ninstead of every time Ido is invoked, so now you can modify it\nlike pretty much every other keymap:\n\n  (define-key ido-common-completion-map\n    (kbd \"C-x g\") \\='ido-enter-magit-status)\n\n(fn)" t nil)

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

(autoload 'magit-add-change-log-entry "magit-extras" "\
Find change log file and add date entry and item for current change.\nThis differs from `add-change-log-entry' (which see) in that\nit acts on the current hunk in a Magit buffer instead of on\na position in a file-visiting buffer.\n\n(fn &optional WHOAMI FILE-NAME OTHER-WINDOW)" t nil)

(autoload 'magit-add-change-log-entry-other-window "magit-extras" "\
Find change log file in other window and add entry and item.\nThis differs from `add-change-log-entry-other-window' (which see)\nin that it acts on the current hunk in a Magit buffer instead of\non a position in a file-visiting buffer.\n\n(fn &optional WHOAMI FILE-NAME)" t nil)

(autoload 'magit-edit-line-commit "magit-extras" "\
Edit the commit that added the current line.\n\nWith a prefix argument edit the commit that removes the line,\nif any.  The commit is determined using `git blame' and made\neditable using `git rebase --interactive' if it is reachable\nfrom `HEAD', or by checking out the commit (or a branch that\npoints at it) otherwise.\n\n(fn &optional TYPE)" t nil)

(autoload 'magit-diff-edit-hunk-commit "magit-extras" "\
From a hunk, edit the respective commit and visit the file.\n\nFirst visit the file being modified by the hunk at the correct\nlocation using `magit-diff-visit-file'.  This actually visits a\nblob.  When point is on a diff header, not within an individual\nhunk, then this visits the blob the first hunk is about.\n\nThen invoke `magit-edit-line-commit', which uses an interactive\nrebase to make the commit editable, or if that is not possible\nbecause the commit is not reachable from `HEAD' by checking out\nthat commit directly.  This also causes the actual worktree file\nto be visited.\n\nNeither the blob nor the file buffer are killed when finishing\nthe rebase.  If that is undesirable, then it might be better to\nuse `magit-rebase-edit-command' instead of this command.\n\n(fn)" t nil)

(autoload 'magit-reshelve-since "magit-extras" "\
Change the author and committer dates of the commits since REV.\n\nAsk the user for the first reachable commit whose dates should\nbe changed.  The read the new date for that commit.  The initial\nminibuffer input and the previous history element offer good\nvalues.  The next commit will be created one minute later and so\non.\n\nThis command is only intended for interactive use and should only\nbe used on highly rearranged and unpublished history.\n\n(fn REV)" t nil)

(autoload 'magit-pop-revision-stack "magit-extras" "\
Insert a representation of a revision into the current buffer.\n\nPop a revision from the `magit-revision-stack' and insert it into\nthe current buffer according to `magit-pop-revision-stack-format'.\nRevisions can be put on the stack using `magit-copy-section-value'\nand `magit-copy-buffer-revision'.\n\nIf the stack is empty or with a prefix argument, instead read a\nrevision in the minibuffer.  By using the minibuffer history this\nallows selecting an item which was popped earlier or to insert an\narbitrary reference or revision without first pushing it onto the\nstack.\n\nWhen reading the revision from the minibuffer, then it might not\nbe possible to guess the correct repository.  When this command\nis called inside a repository (e.g. while composing a commit\nmessage), then that repository is used.  Otherwise (e.g. while\ncomposing an email) then the repository recorded for the top\nelement of the stack is used (even though we insert another\nrevision).  If not called inside a repository and with an empty\nstack, or with two prefix arguments, then read the repository in\nthe minibuffer too.\n\n(fn REV TOPLEVEL)" t nil)

(autoload 'magit-copy-section-value "magit-extras" "\
Save the value of the current section for later use.\n\nSave the section value to the `kill-ring', and, provided that\nthe current section is a commit, branch, or tag section, push\nthe (referenced) revision to the `magit-revision-stack' for use\nwith `magit-pop-revision-stack'.\n\nWhen the current section is a branch or a tag, and a prefix\nargument is used, then save the revision at its tip to the\n`kill-ring' instead of the reference name.\n\nWhen the region is active, then save that to the `kill-ring',\nlike `kill-ring-save' would, instead of behaving as described\nabove.  If a prefix argument is used and the region is within a\nhunk, strip the outer diff marker column.\n\n(fn)" t nil)

(autoload 'magit-copy-buffer-revision "magit-extras" "\
Save the revision of the current buffer for later use.\n\nSave the revision shown in the current buffer to the `kill-ring'\nand push it to the `magit-revision-stack'.\n\nThis command is mainly intended for use in `magit-revision-mode'\nbuffers, the only buffers where it is always unambiguous exactly\nwhich revision should be saved.\n\nMost other Magit buffers usually show more than one revision, in\nsome way or another, so this command has to select one of them,\nand that choice might not always be the one you think would have\nbeen the best pick.\n\nIn such buffers it is often more useful to save the value of\nthe current section instead, using `magit-copy-section-value'.\n\nWhen the region is active, then save that to the `kill-ring',\nlike `kill-ring-save' would, instead of behaving as described\nabove.\n\n(fn)" t nil)

(autoload 'magit-abort-dwim "magit-extras" "\
Abort current operation.\nDepending on the context, this will abort a merge, a rebase, a\npatch application, a cherry-pick, a revert, or a bisect.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-extras" '("magit-")))

;;;***

;;;### (autoloads nil "magit-fetch" "magit-fetch.el" (0 0 0 0))
;;; Generated autoloads from magit-fetch.el
 (autoload 'magit-fetch "magit-fetch" nil t)
 (autoload 'magit-fetch-from-pushremote "magit-fetch" nil t)
 (autoload 'magit-fetch-from-upstream "magit-fetch" nil t)

(autoload 'magit-fetch-other "magit-fetch" "\
Fetch from another repository.\n\n(fn REMOTE ARGS)" t nil)

(autoload 'magit-fetch-branch "magit-fetch" "\
Fetch a BRANCH from a REMOTE.\n\n(fn REMOTE BRANCH ARGS)" t nil)

(autoload 'magit-fetch-refspec "magit-fetch" "\
Fetch a REFSPEC from a REMOTE.\n\n(fn REMOTE REFSPEC ARGS)" t nil)

(autoload 'magit-fetch-all "magit-fetch" "\
Fetch from all remotes.\n\n(fn ARGS)" t nil)

(autoload 'magit-fetch-all-prune "magit-fetch" "\
Fetch from all remotes, and prune.\nPrune remote tracking branches for branches that have been\nremoved on the respective remote.\n\n(fn)" t nil)

(autoload 'magit-fetch-all-no-prune "magit-fetch" "\
Fetch from all remotes.\n\n(fn)" t nil)

(autoload 'magit-fetch-modules "magit-fetch" "\
Fetch all submodules.\n\nOption `magit-fetch-modules-jobs' controls how many submodules\nare being fetched in parallel.  Also fetch the super-repository,\nbecause `git-fetch' does not support not doing that.  With a\nprefix argument fetch all remotes.\n\n(fn &optional ALL)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-fetch" '("magit-")))

;;;***

;;;### (autoloads nil "magit-files" "magit-files.el" (0 0 0 0))
;;; Generated autoloads from magit-files.el

(autoload 'magit-find-file "magit-files" "\
View FILE from REV.\nSwitch to a buffer visiting blob REV:FILE,\ncreating one if none already exists.\n\n(fn REV FILE)" t nil)

(autoload 'magit-find-file-other-window "magit-files" "\
View FILE from REV, in another window.\nLike `magit-find-file', but create a new window or reuse an\nexisting one.\n\n(fn REV FILE)" t nil)
 (autoload 'magit-file-dispatch "magit" nil t)

(defvar global-magit-file-mode t "\
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

;;;### (autoloads nil "magit-gitignore" "magit-gitignore.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from magit-gitignore.el
 (autoload 'magit-gitignore "magit-gitignore" nil t)

(autoload 'magit-gitignore-in-topdir "magit-gitignore" "\
Add the Git ignore RULE to the top-level \".gitignore\" file.\nSince this file is tracked, it is shared with other clones of the\nrepository.  Also stage the file.\n\n(fn RULE)" t nil)

(autoload 'magit-gitignore-in-subdir "magit-gitignore" "\
Add the Git ignore RULE to a \".gitignore\" file.\nPrompted the user for a directory and add the rule to the\n\".gitignore\" file in that directory.  Since such files are\ntracked, they are shared with other clones of the repository.\nAlso stage the file.\n\n(fn RULE DIRECTORY)" t nil)

(autoload 'magit-gitignore-in-gitdir "magit-gitignore" "\
Add the Git ignore RULE to \"$GIT_DIR/info/exclude\".\nRules in that file only affects this clone of the repository.\n\n(fn RULE)" t nil)

(autoload 'magit-gitignore-on-system "magit-gitignore" "\
Add the Git ignore RULE to the file specified by `core.excludesFile'.\nRules that are defined in that file affect all local repositories.\n\n(fn RULE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-gitignore" '("magit-")))

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

(autoload 'magit-log-current "magit-log" "\
Show log for the current branch.\nWhen `HEAD' is detached or with a prefix argument show log for\none or more revs read from the minibuffer.\n\n(fn REVS &optional ARGS FILES)" t nil)

(autoload 'magit-log-other "magit-log" "\
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

(autoload 'magit-log-trace-definition "magit-log" "\
Show log for the definition at point.\n\n(fn FILE FN REV)" t nil)

(autoload 'magit-log-merged "magit-log" "\
Show log for the merge of COMMIT into BRANCH.\nMore precisely, find merge commit M that brought COMMIT into\nBRANCH, and show the log of the range \"M^..M\".  This command\nrequires git-when-merged, which is available from\nhttps://github.com/mhagger/git-when-merged.\n\n(fn COMMIT BRANCH &optional ARGS FILES)" t nil)

(autoload 'magit-reflog-current "magit-log" "\
Display the reflog of the current branch.\n\n(fn ARGS)" t nil)

(autoload 'magit-reflog-other "magit-log" "\
Display the reflog of a branch or another ref.\n\n(fn REF ARGS)" t nil)

(autoload 'magit-reflog-head "magit-log" "\
Display the `HEAD' reflog.\n\n(fn ARGS)" t nil)

(autoload 'magit-log-move-to-parent "magit-log" "\
Move to the Nth parent of the current commit.\n\n(fn &optional N)" t nil)

(autoload 'magit-cherry "magit-log" "\
Show commits in a branch that are not merged in the upstream branch.\n\n(fn HEAD UPSTREAM)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-log" '("magit")))

;;;***

;;;### (autoloads nil "magit-margin" "magit-margin.el" (0 0 0 0))
;;; Generated autoloads from magit-margin.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-margin" '("magit-")))

;;;***

;;;### (autoloads nil "magit-merge" "magit-merge.el" (0 0 0 0))
;;; Generated autoloads from magit-merge.el
 (autoload 'magit-merge "magit" nil t)

(autoload 'magit-merge-plain "magit-merge" "\
Merge commit REV into the current branch; using default message.\n\nUnless there are conflicts or a prefix argument is used create a\nmerge commit using a generic commit message and without letting\nthe user inspect the result.  With a prefix argument pretend the\nmerge failed to give the user the opportunity to inspect the\nmerge.\n\n(git merge --no-edit|--no-commit [ARGS] REV)\n\n(fn REV &optional ARGS NOCOMMIT)" t nil)

(autoload 'magit-merge-editmsg "magit-merge" "\
Merge commit REV into the current branch; and edit message.\nPerform the merge and prepare a commit message but let the user\nedit it.\n\n(git merge --edit --no-ff [ARGS] REV)\n\n(fn REV &optional ARGS)" t nil)

(autoload 'magit-merge-nocommit "magit-merge" "\
Merge commit REV into the current branch; pretending it failed.\nPretend the merge failed to give the user the opportunity to\ninspect the merge and change the commit message.\n\n(git merge --no-commit --no-ff [ARGS] REV)\n\n(fn REV &optional ARGS)" t nil)

(autoload 'magit-merge-into "magit-merge" "\
Merge the current branch into BRANCH and remove the former.\n\nBefore merging, force push the source branch to its push-remote,\nprovided the respective remote branch already exists, ensuring\nthat the respective pull-request (if any) won't get stuck on some\nobsolete version of the commits that are being merged.  Finally\nif `forge-branch-pullreq' was used to create the merged branch,\nbranch, then also remove the respective remote branch.\n\n(fn BRANCH &optional ARGS)" t nil)

(autoload 'magit-merge-absorb "magit-merge" "\
Merge BRANCH into the current branch and remove the former.\n\nBefore merging, force push the source branch to its push-remote,\nprovided the respective remote branch already exists, ensuring\nthat the respective pull-request (if any) won't get stuck on some\nobsolete version of the commits that are being merged.  Finally\nif `forge-branch-pullreq' was used to create the merged branch,\nthen also remove the respective remote branch.\n\n(fn BRANCH &optional ARGS)" t nil)

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
 (autoload 'magit-notes "magit" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-notes" '("magit-")))

;;;***

;;;### (autoloads nil "magit-patch" "magit-patch.el" (0 0 0 0))
;;; Generated autoloads from magit-patch.el
 (autoload 'magit-patch "magit-patch" nil t)
 (autoload 'magit-patch-create "magit-patch" nil t)
 (autoload 'magit-patch-apply "magit-patch" nil t)

(autoload 'magit-patch-save "magit-patch" "\
Write current diff into patch FILE.\n\nWhat arguments are used to create the patch depends on the value\nof `magit-patch-save-arguments' and whether a prefix argument is\nused.\n\nIf the value is the symbol `buffer', then use the same arguments\nas the buffer.  With a prefix argument use no arguments.\n\nIf the value is a list beginning with the symbol `exclude', then\nuse the same arguments as the buffer except for those matched by\nentries in the cdr of the list.  The comparison is done using\n`string-prefix-p'.  With a prefix argument use the same arguments\nas the buffer.\n\nIf the value is a list of strings (including the empty list),\nthen use those arguments.  With a prefix argument use the same\narguments as the buffer.\n\nOf course the arguments that are required to actually show the\nsame differences as those shown in the buffer are always used.\n\n(fn FILE &optional ARG)" t nil)

(autoload 'magit-request-pull "magit-patch" "\
Request upstream to pull from you public repository.\n\nURL is the url of your publically accessible repository.\nSTART is a commit that already is in the upstream repository.\nEND is the last commit, usually a branch name, which upstream\nis asked to pull.  START has to be reachable from that commit.\n\n(fn URL START END)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-patch" '("magit-")))

;;;***

;;;### (autoloads nil "magit-process" "magit-process.el" (0 0 0 0))
;;; Generated autoloads from magit-process.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-process" '("magit-" "tramp-sh-handle-")))

;;;***

;;;### (autoloads nil "magit-pull" "magit-pull.el" (0 0 0 0))
;;; Generated autoloads from magit-pull.el
 (autoload 'magit-pull "magit-pull" nil t)
 (autoload 'magit-pull-from-pushremote "magit-pull" nil t)
 (autoload 'magit-pull-from-upstream "magit-pull" nil t)

(autoload 'magit-pull-branch "magit-pull" "\
Pull from a branch read in the minibuffer.\n\n(fn SOURCE ARGS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-pull" '("magit-")))

;;;***

;;;### (autoloads nil "magit-push" "magit-push.el" (0 0 0 0))
;;; Generated autoloads from magit-push.el
 (autoload 'magit-push "magit-push" nil t)
 (autoload 'magit-push-current-to-pushremote "magit-push" nil t)
 (autoload 'magit-push-current-to-upstream "magit-push" nil t)

(autoload 'magit-push-current "magit-push" "\
Push the current branch to a branch read in the minibuffer.\n\n(fn TARGET ARGS)" t nil)

(autoload 'magit-push-other "magit-push" "\
Push an arbitrary branch or commit somewhere.\nBoth the source and the target are read in the minibuffer.\n\n(fn SOURCE TARGET ARGS)" t nil)

(autoload 'magit-push-refspecs "magit-push" "\
Push one or multiple REFSPECS to a REMOTE.\nBoth the REMOTE and the REFSPECS are read in the minibuffer.  To\nuse multiple REFSPECS, separate them with commas.  Completion is\nonly available for the part before the colon, or when no colon\nis used.\n\n(fn REMOTE REFSPECS ARGS)" t nil)

(autoload 'magit-push-matching "magit-push" "\
Push all matching branches to another repository.\nIf multiple remotes exist, then read one from the user.\nIf just one exists, use that without requiring confirmation.\n\n(fn REMOTE &optional ARGS)" t nil)

(autoload 'magit-push-tags "magit-push" "\
Push all tags to another repository.\nIf only one remote exists, then push to that.  Otherwise prompt\nfor a remote, offering the remote configured for the current\nbranch as default.\n\n(fn REMOTE &optional ARGS)" t nil)

(autoload 'magit-push-tag "magit-push" "\
Push a tag to another repository.\n\n(fn TAG REMOTE &optional ARGS)" t nil)

(autoload 'magit-push-implicitly "magit-push" "\
Push somewhere without using an explicit refspec.\n\nThis command simply runs \"git push -v [ARGS]\".  ARGS are the\narguments specified in the popup buffer.  No explicit refspec\narguments are used.  Instead the behavior depends on at least\nthese Git variables: `push.default', `remote.pushDefault',\n`branch.<branch>.pushRemote', `branch.<branch>.remote',\n`branch.<branch>.merge', and `remote.<remote>.push'.\n\nThe function `magit-push-implicitly--desc' attempts to predict\nwhat this command will do.  The value it returns is displayed in\nthe popup buffer.\n\n(fn ARGS)" t nil)

(autoload 'magit-push-to-remote "magit-push" "\
Push to REMOTE without using an explicit refspec.\nThe REMOTE is read in the minibuffer.\n\nThis command simply runs \"git push -v [ARGS] REMOTE\".  ARGS\nare the arguments specified in the popup buffer.  No refspec\narguments are used.  Instead the behavior depends on at least\nthese Git variables: `push.default', `remote.pushDefault',\n`branch.<branch>.pushRemote', `branch.<branch>.remote',\n`branch.<branch>.merge', and `remote.<remote>.push'.\n\n(fn REMOTE ARGS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-push" '("magit-")))

;;;***

;;;### (autoloads nil "magit-refs" "magit-refs.el" (0 0 0 0))
;;; Generated autoloads from magit-refs.el
 (autoload 'magit-show-refs "magit-refs" nil t)

(autoload 'magit-show-refs-head "magit-refs" "\
List and compare references in a dedicated buffer.\nCompared with `HEAD'.\n\n(fn &optional ARGS)" t nil)

(autoload 'magit-show-refs-current "magit-refs" "\
List and compare references in a dedicated buffer.\nCompare with the current branch or `HEAD' if it is detached.\n\n(fn &optional ARGS)" t nil)

(autoload 'magit-show-refs-other "magit-refs" "\
List and compare references in a dedicated buffer.\nCompared with a branch read from the user.\n\n(fn &optional REF ARGS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-refs" '("magit-")))

;;;***

;;;### (autoloads nil "magit-remote" "magit-remote.el" (0 0 0 0))
;;; Generated autoloads from magit-remote.el
 (autoload 'magit-remote "magit-remote" nil t)

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
 (autoload 'magit-remote-configure "magit-remote" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-remote" '("magit-")))

;;;***

;;;### (autoloads nil "magit-repos" "magit-repos.el" (0 0 0 0))
;;; Generated autoloads from magit-repos.el

(autoload 'magit-list-repositories "magit-repos" "\
Display a list of repositories.\n\nUse the options `magit-repository-directories' to control which\nrepositories are displayed.\n\n(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-repos" '("magit-")))

;;;***

;;;### (autoloads nil "magit-reset" "magit-reset.el" (0 0 0 0))
;;; Generated autoloads from magit-reset.el
 (autoload 'magit-reset "magit" nil t)

(autoload 'magit-reset-mixed "magit-reset" "\
Reset the `HEAD' and index to COMMIT, but not the working tree.\n\n(git reset --mixed COMMIT)\n\n(fn COMMIT)" t nil)

(autoload 'magit-reset-soft "magit-reset" "\
Reset the `HEAD' to COMMIT, but not the index and working tree.\n\n(git reset --soft REVISION)\n\n(fn COMMIT)" t nil)

(autoload 'magit-reset-hard "magit-reset" "\
Reset the `HEAD', index, and working tree to COMMIT.\n\n(git reset --hard REVISION)\n\n(fn COMMIT)" t nil)

(autoload 'magit-reset-index "magit-reset" "\
Reset the index to COMMIT.\nKeep the `HEAD' and working tree as-is, so if COMMIT refers to the\nhead this effectively unstages all changes.\n\n(git reset COMMIT .)\n\n(fn COMMIT)" t nil)

(autoload 'magit-reset-worktree "magit-reset" "\
Reset the worktree to COMMIT.\nKeep the `HEAD' and index as-is.\n\n(fn COMMIT)" t nil)

(autoload 'magit-reset-quickly "magit-reset" "\
Reset the `HEAD' and index to COMMIT, and possibly the working tree.\nWith a prefix argument reset the working tree otherwise don't.\n\n(git reset --mixed|--hard COMMIT)\n\n(fn COMMIT &optional HARD)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-reset" '("magit-reset")))

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
 (autoload 'magit-cherry-pick "magit-sequence" nil t)

(autoload 'magit-cherry-copy "magit-sequence" "\
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
 (autoload 'magit-revert "magit-sequence" nil t)

(autoload 'magit-revert-and-commit "magit-sequence" "\
Revert COMMIT by creating a new commit.\nPrompt for a commit, defaulting to the commit at point.  If\nthe region selects multiple commits, then revert all of them,\nwithout prompting.\n\n(fn COMMIT &optional ARGS)" t nil)

(autoload 'magit-revert-no-commit "magit-sequence" "\
Revert COMMIT by applying it in reverse to the worktree.\nPrompt for a commit, defaulting to the commit at point.  If\nthe region selects multiple commits, then revert all of them,\nwithout prompting.\n\n(fn COMMIT &optional ARGS)" t nil)
 (autoload 'magit-am "magit-sequence" nil t)

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
 (autoload 'magit-rebase "magit-sequence" nil t)
 (autoload 'magit-rebase-onto-pushremote "magit-sequence" nil t)
 (autoload 'magit-rebase-onto-upstream "magit-sequence" nil t)

(autoload 'magit-rebase-branch "magit-sequence" "\
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
 (autoload 'magit-stash "magit-stash" nil t)

(autoload 'magit-stash-both "magit-stash" "\
Create a stash of the index and working tree.\nUntracked files are included according to infix arguments.\nOne prefix argument is equivalent to `--include-untracked'\nwhile two prefix arguments are equivalent to `--all'.\n\n(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-stash-index "magit-stash" "\
Create a stash of the index only.\nUnstaged and untracked changes are not stashed.  The stashed\nchanges are applied in reverse to both the index and the\nworktree.  This command can fail when the worktree is not clean.\nApplying the resulting stash has the inverse effect.\n\n(fn MESSAGE)" t nil)

(autoload 'magit-stash-worktree "magit-stash" "\
Create a stash of unstaged changes in the working tree.\nUntracked files are included according to infix arguments.\nOne prefix argument is equivalent to `--include-untracked'\nwhile two prefix arguments are equivalent to `--all'.\n\n(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-stash-keep-index "magit-stash" "\
Create a stash of the index and working tree, keeping index intact.\nUntracked files are included according to infix arguments.\nOne prefix argument is equivalent to `--include-untracked'\nwhile two prefix arguments are equivalent to `--all'.\n\n(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-snapshot-both "magit-stash" "\
Create a snapshot of the index and working tree.\nUntracked files are included according to infix arguments.\nOne prefix argument is equivalent to `--include-untracked'\nwhile two prefix arguments are equivalent to `--all'.\n\n(fn &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-snapshot-index "magit-stash" "\
Create a snapshot of the index only.\nUnstaged and untracked changes are not stashed.\n\n(fn)" t nil)

(autoload 'magit-snapshot-worktree "magit-stash" "\
Create a snapshot of unstaged changes in the working tree.\nUntracked files are included according to infix arguments.\nOne prefix argument is equivalent to `--include-untracked'\nwhile two prefix arguments are equivalent to `--all'.\n\n(fn &optional INCLUDE-UNTRACKED)" t nil)

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
Show the status of the current Git repository in a buffer.\n\nIf the current directory isn't located within a Git repository,\nthen prompt for an existing repository or an arbitrary directory,\ndepending on option `magit-repository-directories', and show the\nstatus of the selected repository instead.\n\n* If that option specifies any existing repositories, then offer\n  those for completion and show the status buffer for the\n  selected one.\n\n* Otherwise read an arbitrary directory using regular file-name\n  completion.  If the selected directory is the top-level of an\n  existing working tree, then show the status buffer for that.\n\n* Otherwise offer to initialize the selected directory as a new\n  repository.  After creating the repository show its status\n  buffer.\n\nThese fallback behaviors can also be forced using one or more\nprefix arguments:\n\n* With two prefix arguments (or more precisely a numeric prefix\n  value of 16 or greater) read an arbitrary directory and act on\n  it as described above.  The same could be accomplished using\n  the command `magit-init'.\n\n* With a single prefix argument read an existing repository, or\n  if none can be found based on `magit-repository-directories',\n  then fall back to the same behavior as with two prefix\n  arguments.\n\n(fn &optional DIRECTORY CACHE)" t nil)

(autoload 'magit-status-internal "magit-status" "\
\n\n(fn DIRECTORY)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-status" '("magit")))

;;;***

;;;### (autoloads nil "magit-submodule" "magit-submodule.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from magit-submodule.el
 (autoload 'magit-submodule "magit-submodule" nil t)
 (autoload 'magit-submodule-add "magit-submodule" nil t)

(autoload 'magit-submodule-read-name-for-path "magit-submodule" "\
\n\n(fn PATH &optional PREFER-SHORT)" nil nil)
 (autoload 'magit-submodule-register "magit-submodule" nil t)
 (autoload 'magit-submodule-populate "magit-submodule" nil t)
 (autoload 'magit-submodule-update "magit-submodule" nil t)
 (autoload 'magit-submodule-synchronize "magit-submodule" nil t)
 (autoload 'magit-submodule-unpopulate "magit-submodule" nil t)

(autoload 'magit-submodule-remove "magit-submodule" "\
Unregister MODULES and remove their working directories.\n\nFor safety reasons, do not remove the gitdirs and if a module has\nuncomitted changes, then do not remove it at all.  If a module's\ngitdir is located inside the working directory, then move it into\nthe gitdir of the superproject first.\n\nWith the \"--force\" argument offer to remove dirty working\ndirectories and with a prefix argument offer to delete gitdirs.\nBoth actions are very dangerous and have to be confirmed.  There\nare additional safety precautions in place, so you might be able\nto recover from making a mistake here, but don't count on it.\n\n(fn MODULES ARGS TRASH-GITDIRS)" t nil)

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
 (autoload 'magit-subtree "magit-subtree" nil t)
 (autoload 'magit-subtree-import "magit-subtree" nil t)
 (autoload 'magit-subtree-export "magit-subtree" nil t)

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
 (autoload 'magit-tag "magit" nil t)

(autoload 'magit-tag-create "magit-tag" "\
Create a new tag with the given NAME at REV.\nWith a prefix argument annotate the tag.\n\n(git tag [--annotate] NAME REV)\n\n(fn NAME REV &optional ARGS)" t nil)

(autoload 'magit-tag-delete "magit-tag" "\
Delete one or more tags.\nIf the region marks multiple tags (and nothing else), then offer\nto delete those, otherwise prompt for a single tag to be deleted,\ndefaulting to the tag at point.\n\n(git tag -d TAGS)\n\n(fn TAGS)" t nil)

(autoload 'magit-tag-prune "magit-tag" "\
Offer to delete tags missing locally from REMOTE, and vice versa.\n\n(fn TAGS REMOTE-TAGS REMOTE)" t nil)

(autoload 'magit-tag-release "magit-tag" "\
Create an annotated release tag.\n\nAssume that release tags match `magit-release-tag-regexp'.\n\nFirst prompt for the name of the new tag using the highest\nexisting tag as initial input and leaving it to the user to\nincrement the desired part of the version string.\n\nThen prompt for the message of the new tag.  Base the proposed\ntag message on the message of the highest tag, provided that\nthat contains the corresponding version string and substituting\nthe new version string for that.  Otherwise propose something\nlike \"Foo-Bar 1.2.3\", given, for example, a TAG \"v1.2.3\" and a\nrepository located at something like \"/path/to/foo-bar\".\n\nThen call \"git tag --annotate --sign -m MSG TAG\" to create the,\ntag, regardless of whether these arguments are enabled in the\npopup.  Finally show the refs buffer to let the user quickly\nreview the result.\n\n(fn TAG MSG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-tag" '("magit-")))

;;;***

;;;### (autoloads nil "magit-transient" "magit-transient.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from magit-transient.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-transient" '("magit-")))

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

(autoload 'magit-wip-commit-initial-backup "magit-wip" "\
Before saving, commit current file to a worktree wip ref.\n\nThe user has to add this function to `before-save-hook'.\n\nCommit the current state of the visited file before saving the\ncurrent buffer to that file.  This backs up the same version of\nthe file as `backup-buffer' would, but stores the backup in the\nworktree wip ref, which is also used by the various Magit Wip\nmodes, instead of in a backup file as `backup-buffer' would.\n\nThis function ignores the variables that affect `backup-buffer'\nand can be used along-side that function, which is recommended\nbecause this function only backs up files that are tracked in\na Git repository.\n\n(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-wip" '("magit-")))

;;;***

;;;### (autoloads nil "magit-worktree" "magit-worktree.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from magit-worktree.el
 (autoload 'magit-worktree "magit-worktree" nil t)

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
