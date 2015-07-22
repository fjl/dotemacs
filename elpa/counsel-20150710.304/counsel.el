;;; counsel.el --- Various completion functions using Ivy -*- lexical-binding: t -*-

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/swiper
;; Package-Version: 20150710.304
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1") (swiper "0.4.0"))
;; Keywords: completion, matching

;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Just call one of the interactive functions in this file to complete
;; the corresponding thing using `ivy'.
;;
;; Currently available: Elisp symbols, Clojure symbols, Git files.

;;; Code:

(require 'swiper)

(defvar counsel-completion-beg nil
  "Completion bounds start.")

(defvar counsel-completion-end nil
  "Completion bounds end.")

;;;###autoload
(defun counsel-el ()
  "Elisp completion at point."
  (interactive)
  (let* ((bnd (unless (and (looking-at ")")
                           (eq (char-before) ?\())
                (bounds-of-thing-at-point
                 'symbol)))
         (str (if bnd
                  (buffer-substring-no-properties
                   (car bnd)
                   (cdr bnd))
                ""))
         (ivy-height 7)
         (funp (eq (char-before (car bnd)) ?\())
         symbol-names)
    (if bnd
        (progn
          (setq counsel-completion-beg
                (move-marker (make-marker) (car bnd)))
          (setq counsel-completion-end
                (move-marker (make-marker) (cdr bnd))))
      (setq counsel-completion-beg nil)
      (setq counsel-completion-end nil))
    (if (string= str "")
        (mapatoms
         (lambda (x)
           (when (symbolp x)
             (push (symbol-name x) symbol-names))))
      (setq symbol-names
            (all-completions str obarray
                             (and funp
                                  (lambda (x)
                                    (or (functionp x)
                                        (macrop x)
                                        (special-form-p x)))))))
    (ivy-read "Symbol name: " symbol-names
              :predicate (and funp #'functionp)
              :initial-input str
              :action #'counsel--el-action)))

(defun counsel--el-action (symbol)
  "Insert SYMBOL, erasing the previous one."
  (when (stringp symbol)
    (when counsel-completion-beg
      (delete-region
       counsel-completion-beg
       counsel-completion-end))
    (setq counsel-completion-beg
          (move-marker (make-marker) (point)))
    (insert symbol)
    (setq counsel-completion-end
          (move-marker (make-marker) (point)))))

(defvar counsel-describe-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-.") #'counsel-find-symbol)
    (define-key map (kbd "C-,") #'counsel--info-lookup-symbol)
    map))

(defun counsel-find-symbol ()
  "Jump to the definition of the current symbol."
  (interactive)
  (ivy-set-action #'counsel--find-symbol)
  (ivy-done))

(defun counsel--info-lookup-symbol ()
  "Lookup the current symbol in the info docs."
  (interactive)
  (ivy-set-action #'counsel-info-lookup-symbol)
  (ivy-done))

(defun counsel--find-symbol (x)
  "Find symbol definition that corresponds to string X."
  (let ((full-name (get-text-property 0 'full-name x)))
    (if full-name
        (find-library full-name)
      (let ((sym (read x)))
        (cond ((boundp sym)
               (find-variable sym))
              ((fboundp sym)
               (find-function sym))
              ((or (featurep sym)
                   (locate-library
                    (prin1-to-string sym)))
               (find-library
                (prin1-to-string sym)))
              (t
               (error "Couldn't fild definition of %s"
                      sym)))))))

(defvar counsel-describe-symbol-history nil
  "History for `counsel-describe-variable' and `counsel-describe-function'.")

(defun counsel-symbol-at-point ()
  "Return current symbol at point as a string."
  (let ((s (thing-at-point 'symbol)))
    (and (stringp s)
         (if (string-match "\\`[`']?\\(.*\\)'?\\'" s)
             (match-string 1 s)
           s))))

;;;###autoload
(defun counsel-describe-variable ()
  "Forward to `describe-variable'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (ivy-read
     "Describe variable: "
     (let (cands)
       (mapatoms
        (lambda (vv)
          (when (or (get vv 'variable-documentation)
                    (and (boundp vv) (not (keywordp vv))))
            (push (symbol-name vv) cands))))
       cands)
     :keymap counsel-describe-map
     :preselect (counsel-symbol-at-point)
     :history 'counsel-describe-symbol-history
     :require-match t
     :sort t
     :action (lambda (x)
               (describe-variable
                (intern x))))))

;;;###autoload
(defun counsel-describe-function ()
  "Forward to `describe-function'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (ivy-read "Describe function: "
              (let (cands)
                (mapatoms
                 (lambda (x)
                   (when (fboundp x)
                     (push (symbol-name x) cands))))
                cands)
              :keymap counsel-describe-map
              :preselect (counsel-symbol-at-point)
              :history 'counsel-describe-symbol-history
              :require-match t
              :sort t
              :action (lambda (x)
                        (describe-function
                         (intern x))))))

(defvar info-lookup-mode)
(declare-function info-lookup->completions "info-look")
(declare-function info-lookup->mode-value "info-look")
(declare-function info-lookup-select-mode "info-look")
(declare-function info-lookup-change-mode "info-look")
(declare-function info-lookup "info-look")

;;;###autoload
(defun counsel-info-lookup-symbol (symbol &optional mode)
  "Forward to (`info-describe-symbol' SYMBOL MODE) with ivy completion."
  (interactive
   (progn
     (require 'info-look)
     (let* ((topic 'symbol)
            (mode (cond (current-prefix-arg
                         (info-lookup-change-mode topic))
                        ((info-lookup->mode-value
                          topic (info-lookup-select-mode))
                         info-lookup-mode)
                        ((info-lookup-change-mode topic))))
            (completions (info-lookup->completions topic mode))
            (enable-recursive-minibuffers t)
            (value (ivy-read
                    "Describe symbol: "
                    (mapcar #'car completions)
                    :sort t)))
       (list value info-lookup-mode))))
  (require 'info-look)
  (info-lookup 'symbol symbol mode))

;;;###autoload
(defun counsel-unicode-char ()
  "Insert a Unicode character at point."
  (interactive)
  (let ((minibuffer-allow-text-properties t))
    (ivy-read "Unicode name: "
              (mapcar (lambda (x)
                        (propertize
                         (format "% -60s%c" (car x) (cdr x))
                         'result (cdr x)))
                      (ucs-names))
              :action (lambda (char)
                        (insert-char (get-text-property 0 'result char))))))

(declare-function cider-sync-request:complete "ext:cider-client")
;;;###autoload
(defun counsel-clj ()
  "Clojure completion at point."
  (interactive)
  (counsel--generic
   (lambda (str)
     (mapcar
      #'cl-caddr
      (cider-sync-request:complete str ":same")))))

;;;###autoload
(defun counsel-git ()
  "Find file in the current Git repository."
  (interactive)
  (let* ((default-directory (locate-dominating-file
                             default-directory ".git"))
         (cands (split-string
                 (shell-command-to-string
                  "git ls-files --full-name --")
                 "\n"
                 t))
         (action (lambda (x) (find-file x))))
    (ivy-read "Find file: " cands
              :action action)))

(defvar counsel--git-grep-dir nil
  "Store the base git directory.")

(defvar counsel--git-grep-count nil
  "Store the line count in current repository.")

(defun counsel-git-grep-function (string &optional _pred &rest _unused)
  "Grep in the current git repository for STRING."
  (if (and (> counsel--git-grep-count 20000)
           (< (length string) 3))
      (progn
        (setq ivy--full-length counsel--git-grep-count)
        (list ""
              (format "%d chars more" (- 3 (length ivy-text)))))
    (let* ((default-directory counsel--git-grep-dir)
           (cmd (format "git --no-pager grep --full-name -n --no-color -i -e %S"
                        (ivy--regex string t)))
           res)
      (if (<= counsel--git-grep-count 20000)
          (progn
            (setq res (shell-command-to-string cmd))
            (setq ivy--full-length nil)
            (split-string res "\n" t))
        (setq ivy--full-length -1)
        (counsel--gg-candidates (ivy--regex string))
        nil))))

(defvar counsel-git-grep-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") 'counsel-git-grep-recenter)
    map))

(defun counsel-git-grep-recenter ()
  (interactive)
  (with-selected-window (ivy-state-window ivy-last)
    (counsel-git-grep-action ivy--current)
    (recenter-top-bottom)))

(defun counsel-git-grep-action (x)
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (let ((file-name (match-string-no-properties 1 x))
          (line-number (match-string-no-properties 2 x)))
      (find-file (expand-file-name file-name counsel--git-grep-dir))
      (goto-char (point-min))
      (forward-line (1- (string-to-number line-number)))
      (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
      (unless (eq ivy-exit 'done)
        (setq swiper--window (selected-window))
        (swiper--cleanup)
        (swiper--add-overlays (ivy--regex ivy-text))))))

(defvar counsel-git-grep-history nil
  "History for `counsel-git-grep'.")

;;;###autoload
(defun counsel-git-grep (&optional initial-input)
  "Grep for a string in the current git repository."
  (interactive)
  (setq counsel--git-grep-dir
        (locate-dominating-file default-directory ".git"))
  (if (null counsel--git-grep-dir)
      (error "Not in a git repository")
    (setq counsel--git-grep-count (counsel--gg-count "" t))
    (ivy-read "pattern: " 'counsel-git-grep-function
              :initial-input initial-input
              :matcher #'counsel-git-grep-matcher
              :dynamic-collection (when (> counsel--git-grep-count 20000)
                                    'counsel-git-grep-function)
              :keymap counsel-git-grep-map
              :action #'counsel-git-grep-action
              :unwind #'swiper--cleanup
              :history 'counsel-git-grep-history)))

(defcustom counsel-find-file-at-point nil
  "When non-nil, add file-at-point to the list of candidates."
  :type 'boolean
  :group 'ivy)

(declare-function ffap-guesser "ffap")

(defvar counsel-find-file-map (make-sparse-keymap))

;;;###autoload
(defun counsel-find-file ()
  "Forward to `find-file'."
  (interactive)
  (ivy-read "Find file: " 'read-file-name-internal
            :matcher #'counsel--find-file-matcher
            :action
            (lambda (x)
              (find-file (expand-file-name x ivy--directory)))
            :preselect (when counsel-find-file-at-point
                         (require 'ffap)
                         (ffap-guesser))
            :require-match 'confirm-after-completion
            :history 'file-name-history
            :keymap counsel-find-file-map))

(defcustom counsel-find-file-ignore-regexp nil
  "A regexp of files to ignore while in `counsel-find-file'.
These files are un-ignored if `ivy-text' matches them.
The common way to show all files is to start `ivy-text' with a dot.
Possible value: \"\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)\"."
  :group 'ivy)

(defun counsel--find-file-matcher (regexp candidates)
  "Return REGEXP-matching CANDIDATES.
Skip some dotfiles unless `ivy-text' requires them."
  (let ((res (cl-remove-if-not
              (lambda (x)
                (string-match regexp x))
              candidates)))
    (if (or (null counsel-find-file-ignore-regexp)
            (string-match counsel-find-file-ignore-regexp ivy-text))
        res
      (cl-remove-if
       (lambda (x)
         (string-match counsel-find-file-ignore-regexp x))
       res))))

(defun counsel-git-grep-matcher (regexp candidates)
  (or (and (equal regexp ivy--old-re)
           ivy--old-cands)
      (prog1
          (setq ivy--old-cands
                (cl-remove-if-not
                 (lambda (x)
                   (ignore-errors
                     (when (string-match "^[^:]+:[^:]+:" x)
                       (setq x (substring x (match-end 0)))
                       (if (stringp regexp)
                           (string-match regexp x)
                         (let ((res t))
                           (dolist (re regexp)
                             (setq res
                                   (and res
                                        (ignore-errors
                                          (if (cdr re)
                                              (string-match (car re) x)
                                            (not (string-match (car re) x)))))))
                           res)))))
                 candidates))
        (setq ivy--old-re regexp))))

(defun counsel-locate-function (str &rest _u)
  (if (< (length str) 3)
      (list ""
            (format "%d chars more" (- 3 (length ivy-text))))
    (counsel--async-command
     (concat "locate -i --regex " (ivy--regex str)))))

(defun counsel--async-command (cmd)
  (let* ((counsel--process " *counsel*")
         (proc (get-process counsel--process))
         (buff (get-buffer counsel--process)))
    (when proc
      (delete-process proc))
    (when buff
      (kill-buffer buff))
    (setq proc (start-process-shell-command
                counsel--process
                counsel--process
                cmd))
    (set-process-sentinel proc #'counsel--async-sentinel)))

(defun counsel--async-sentinel (process event)
  (if (string= event "finished\n")
      (progn
        (with-current-buffer (process-buffer process)
          (setq ivy--all-candidates (split-string (buffer-string) "\n" t))
          (setq ivy--old-cands ivy--all-candidates))
        (ivy--insert-minibuffer
         (ivy--format ivy--all-candidates)))
    (if (string= event "exited abnormally with code 1\n")
        (message "Error"))))

(defun counsel-locate-action-extern (x)
  "Use xdg-open shell command on X."
  (call-process shell-file-name nil
                nil nil
                shell-command-switch
                (format "xdg-open %s" (shell-quote-argument x))))

(declare-function dired-jump "dired-x")
(defun counsel-locate-action-dired (x)
  "Use `dired-jump' on X."
  (dired-jump nil x))

(defvar counsel-locate-history nil
  "History for `counsel-locate'.")

(ivy-set-actions
 'counsel-locate
 '(("xdg-open" counsel-locate-action-extern)
   ("dired" counsel-locate-action-dired)))

;;;###autoload
(defun counsel-locate ()
  "Call locate shell command."
  (interactive)
  (ivy-read "Locate: " nil
            :dynamic-collection #'counsel-locate-function
            :history 'counsel-locate-history
            :action
            (lambda (val)
              (when val
                (find-file val)))))

(defun counsel--generic (completion-fn)
  "Complete thing at point with COMPLETION-FN."
  (let* ((bnd (bounds-of-thing-at-point 'symbol))
         (str (if bnd
                  (buffer-substring-no-properties
                   (car bnd) (cdr bnd))
                ""))
         (candidates (funcall completion-fn str))
         (ivy-height 7)
         (res (ivy-read (format "pattern (%s): " str)
                        candidates)))
    (when (stringp res)
      (when bnd
        (delete-region (car bnd) (cdr bnd)))
      (insert res))))

(defun counsel-directory-parent (dir)
  "Return the directory parent of directory DIR."
  (concat (file-name-nondirectory
           (directory-file-name dir)) "/"))

(defun counsel-string-compose (prefix str)
  "Make PREFIX the display prefix of STR though text properties."
  (let ((str (copy-sequence str)))
    (put-text-property
     0 1 'display
     (concat prefix (substring str 0 1))
     str)
    str))

;;;###autoload
(defun counsel-load-library ()
  "Load a selected the Emacs Lisp library.
The libraries are offered from `load-path'."
  (interactive)
  (let ((dirs load-path)
        (suffix (concat (regexp-opt '(".el" ".el.gz") t) "\\'"))
        (cands (make-hash-table :test #'equal))
        short-name
        old-val
        dir-parent
        res)
    (dolist (dir dirs)
      (when (file-directory-p dir)
        (dolist (file (file-name-all-completions "" dir))
          (when (string-match suffix file)
            (unless (string-match "pkg.elc?$" file)
              (setq short-name (substring file 0 (match-beginning 0)))
              (if (setq old-val (gethash short-name cands))
                  (progn
                    ;; assume going up directory once will resolve name clash
                    (setq dir-parent (counsel-directory-parent (cdr old-val)))
                    (puthash short-name
                             (cons
                              (counsel-string-compose dir-parent (car old-val))
                              (cdr old-val))
                             cands)
                    (setq dir-parent (counsel-directory-parent dir))
                    (puthash (concat dir-parent short-name)
                             (cons
                              (propertize
                               (counsel-string-compose
                                dir-parent short-name)
                               'full-name (expand-file-name file dir))
                              dir)
                             cands))
                (puthash short-name
                         (cons (propertize
                                short-name
                                'full-name (expand-file-name file dir))
                               dir) cands)))))))
    (maphash (lambda (_k v) (push (car v) res)) cands)
    (ivy-read "Load library: " (nreverse res)
              :action (lambda (x)
                        (load-library
                         (get-text-property 0 'full-name x)))
              :keymap counsel-describe-map)))

(defun counsel--gg-candidates (regex)
  "Return git grep candidates for REGEX."
  (counsel--gg-count regex)
  (let* ((default-directory counsel--git-grep-dir)
         (counsel-gg-process " *counsel-gg*")
         (proc (get-process counsel-gg-process))
         (buff (get-buffer counsel-gg-process)))
    (when proc
      (delete-process proc))
    (when buff
      (kill-buffer buff))
    (setq proc (start-process-shell-command
                counsel-gg-process
                counsel-gg-process
                (format "git --no-pager grep --full-name -n --no-color -i -e %S | head -n 200"
                        regex)))
    (set-process-sentinel
     proc
     #'counsel--gg-sentinel)))

(defun counsel--gg-sentinel (process event)
  (if (string= event "finished\n")
      (progn
        (with-current-buffer (process-buffer process)
          (setq ivy--all-candidates (split-string (buffer-string) "\n" t))
          (setq ivy--old-cands ivy--all-candidates))
        (unless (eq ivy--full-length -1)
          (ivy--insert-minibuffer
           (ivy--format ivy--all-candidates))))
    (if (string= event "exited abnormally with code 1\n")
        (message "Error"))))

(defun counsel--gg-count (regex &optional no-async)
  "Quickly and asynchronously count the amount of git grep REGEX matches.
When NO-ASYNC is non-nil, do it synchronously."
  (let ((default-directory counsel--git-grep-dir)
        (cmd (format "git grep -i -c '%s' | sed 's/.*:\\(.*\\)/\\1/g' | awk '{s+=$1} END {print s}'"
                     regex))
        (counsel-ggc-process " *counsel-gg-count*"))
    (if no-async
        (string-to-number (shell-command-to-string cmd))
      (let ((proc (get-process counsel-ggc-process))
            (buff (get-buffer counsel-ggc-process)))
        (when proc
          (delete-process proc))
        (when buff
          (kill-buffer buff))
        (setq proc (start-process-shell-command
                    counsel-ggc-process
                    counsel-ggc-process
                    cmd))
        (set-process-sentinel
         proc
         #'(lambda (process event)
             (when (string= event "finished\n")
               (with-current-buffer (process-buffer process)
                 (setq ivy--full-length (string-to-number (buffer-string))))
               (ivy--insert-minibuffer
                (ivy--format ivy--all-candidates)))))))))

(defun counsel--M-x-transformer (cmd)
  "Add a binding to CMD if it's bound in the current window.
CMD is a command name."
  (let ((binding (substitute-command-keys (format "\\[%s]" cmd))))
    (setq binding (replace-regexp-in-string "C-x 6" "<f2>" binding))
    (if (string-match "^M-x" binding)
        cmd
      (format "%s (%s)" cmd
              (propertize binding 'face 'font-lock-keyword-face)))))

(defvar smex-initialized-p)
(defvar smex-ido-cache)
(declare-function smex-initialize "ext:smex")
(declare-function smex-detect-new-commands "ext:smex")
(declare-function smex-update "ext:smex")
(declare-function smex-rank "ext:smex")
(declare-function package-installed-p "package")

;;;###autoload
(defun counsel-M-x (&optional initial-input)
  "Ivy version of `execute-extended-command'.
Optional INITIAL-INPUT is the initial input in the minibuffer."
  (interactive)
  (unless initial-input
    (setq initial-input (cdr (assoc this-command
                                    ivy-initial-inputs-alist))))
  (let* ((store ivy-format-function)
         (ivy-format-function
          (lambda (cands)
            (funcall
             store
             (with-selected-window (ivy-state-window ivy-last)
               (mapcar #'counsel--M-x-transformer cands)))))
         (cands obarray)
         (pred 'commandp)
         (sort t))
    (when (or (featurep 'smex)
              (package-installed-p 'smex))
      (require 'smex)
      (unless smex-initialized-p
        (smex-initialize))
      (smex-detect-new-commands)
      (smex-update)
      (setq cands smex-ido-cache)
      (setq pred nil)
      (setq sort nil))
    (ivy-read "M-x " cands
              :predicate pred
              :require-match t
              :history 'extended-command-history
              :action
              (lambda (cmd)
                (when (featurep 'smex)
                  (smex-rank (intern cmd)))
                (let ((prefix-arg current-prefix-arg))
                  (command-execute (intern cmd) 'record)))
              :sort sort
              :keymap counsel-describe-map
              :initial-input initial-input)))

(declare-function powerline-reset "ext:powerline")

(defun counsel--load-theme-action (x)
  "Disable current themes and load theme X."
  (condition-case nil
      (progn
        (mapc #'disable-theme custom-enabled-themes)
        (load-theme (intern x))
        (when (fboundp 'powerline-reset)
          (powerline-reset)))
    (error "Problem loading theme %s" x)))

;;;###autoload
(defun counsel-load-theme ()
  "Forward to `load-theme'.
Usable with `ivy-resume', `ivy-next-line-and-call' and
`ivy-previous-line-and-call'."
  (interactive)
  (ivy-read "Load custom theme: "
            (mapcar 'symbol-name
                    (custom-available-themes))
            :action #'counsel--load-theme-action))

(defvar rhythmbox-library)
(declare-function rhythmbox-load-library "ext:helm-rhythmbox")
(declare-function dbus-call-method "dbus")
(declare-function rhythmbox-song-uri "ext:helm-rhythmbox")
(declare-function helm-rhythmbox-candidates "ext:helm-rhythmbox")

(defun counsel-rhythmbox-enqueue-song (song)
  "Let Rhythmbox enqueue SONG."
  (let ((service "org.gnome.Rhythmbox3")
        (path "/org/gnome/Rhythmbox3/PlayQueue")
        (interface "org.gnome.Rhythmbox3.PlayQueue"))
    (dbus-call-method :session service path interface
                      "AddToQueue" (rhythmbox-song-uri song))))

(defvar counsel-rhythmbox-history nil
  "History for `counsel-rhythmbox'.")

;;;###autoload
(defun counsel-rhythmbox ()
  "Choose a song from the Rhythmbox library to play or enqueue."
  (interactive)
  (unless (require 'helm-rhythmbox nil t)
    (error "Please install `helm-rhythmbox'"))
  (unless rhythmbox-library
    (rhythmbox-load-library)
    (while (null rhythmbox-library)
      (sit-for 0.1)))
  (ivy-read "Rhythmbox: "
            (helm-rhythmbox-candidates)
            :history 'counsel-rhythmbox-history
            :action
            '(1
              ("Play song" helm-rhythmbox-play-song)
              ("Enqueue song" counsel-rhythmbox-enqueue-song))))

(provide 'counsel)

;;; counsel.el ends here
