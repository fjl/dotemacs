(require 'init-bootstrap)
(require 'eshell)
(require 'em-prompt)
(require 'em-hist)
(require 'ring)
(require 'levenshtein)

;; Load 'z' extension. This tracks frequently-used directories and
;; enables jumping into them using the 'z' command.
(require 'eshell-z)

;; Shell Ring. Tracks eshells and whether they are currently in use,
;; so that I can get a 'free' shell whenever I want.

(defvar fjl/free-eshells nil
  "This list tracks eshell buffers which are not currently executing
a command. They are kept in no particular order.")
(defvar fjl/busy-eshells nil
  "This list tracks eshell buffers in which a command is running.
They are orded by the time when the command was started (most recent first).")

(defvar fjl/eshell-id-counter 0)

;;;###autoload
(defun switch-to-eshell (&optional close-to-dir other-window)
  "Switches to a non-busy eshell."
  (interactive)
  (unless close-to-dir
    (setq close-to-dir default-directory))
  (if (null fjl/free-eshells)
      ;; There are no free shells, just make a new one.
      (switch-to-buffer (fjl/new-eshell close-to-dir))
    ;; Try to select a shell which is already in the right directory,
    ;; or at least in a directory that's "close".
      (setq fjl/free-eshells
            (cl-sort fjl/free-eshells '<
                     :key (lambda (b)
                            (levenshtein-distance close-to-dir
                                                  (buffer-local-value 'default-directory b)))))
      (let* ((buf (car fjl/free-eshells))
             (win (get-buffer-window buf 'visible)))
        (cond (win          (select-window win))
              (other-window (switch-to-buffer-other-window buf))
              (t            (switch-to-buffer buf))))))

;;;###autoload
(defun switch-to-eshell-other-window (&optional close-to-dir)
  (interactive)
  (switch-to-eshell close-to-dir t))

;;;###autoload
(defun switch-to-eshell-in-directory (&optional dir)
  "Switches to a non-busy eshell, switching it to the working
directory of the current buffer."
  (interactive)
  (when (null dir)
    (setq dir default-directory))
  (switch-to-eshell dir)
  (fjl/eshell-cd-and-reprompt dir))

;;;###autoload
(defun switch-to-eshell-z ()
  "Prompts for a recent directory, then switches to a non-busy
eshell in that directory."
  (interactive)
  (let* ((paths
          (sort (eshell-z--hash-table-values eshell-z-freq-dir-hash-table)
                (lambda (elt1 elt2)
                  (> (eshell-z--frecent elt1)
                     (eshell-z--frecent elt2)))))
         (dir (completing-read "eshell-z " paths nil t)))
    (switch-to-eshell-in-directory dir)))

(defun fjl/eshell-cd-and-reprompt (dir)
  (eshell/cd dir)
  (goto-char (point-max))
  (save-match-data
    (when (looking-back eshell-prompt-regexp
                        (max (point-min) (- (point) 200)))
      (let ((inhibit-read-only t))
        (delete-region (match-beginning 0) (match-end 0))
        (eshell-emit-prompt)))))

(defun fjl/new-eshell (&optional working-dir)
  (let ((buffer (get-buffer-create (format "*autoeshell %d*" fjl/eshell-id-counter))))
    (cl-incf fjl/eshell-id-counter)
    (push buffer fjl/free-eshells)
    (with-current-buffer buffer
      (when (and working-dir (file-accessible-directory-p working-dir))
        (setq-local default-directory working-dir))
      (eshell-mode)
      (rename-buffer "Eshell" t))
    buffer))

(defmacro fjl/deletef (element listvar)
  `(setq ,listvar (delete ,element ,listvar)))

(defun fjl/remove-from-eshell-lists ()
  (fjl/deletef (current-buffer) fjl/busy-eshells)
  (fjl/deletef (current-buffer) fjl/free-eshells))

(defun fjl/mark-eshell-buffer-as-free ()
  (fjl/deletef (current-buffer) fjl/busy-eshells)
  (push (current-buffer) fjl/free-eshells))

(defun fjl/mark-eshell-buffer-as-busy ()
  (fjl/deletef (current-buffer) fjl/free-eshells)
  (push (current-buffer) fjl/busy-eshells))

(defun fjl/setup-eshell-list-hooks ()
  (fjl/mark-eshell-buffer-as-free)
  (add-hook 'kill-buffer-hook 'fjl/remove-from-eshell-lists)
  (add-hook 'eshell-pre-command-hook 'fjl/mark-eshell-buffer-as-busy)
  (add-hook 'eshell-post-command-hook 'fjl/mark-eshell-buffer-as-free))

;;;###autoload
(add-hook 'eshell-mode-hook 'fjl/setup-eshell-list-hooks)

(defun fjl/eshell-clear ()
  "Clear the eshell buffer, deleting everything except the last prompt."
  (interactive)
  (if (not eshell-mode)
      (message "Not in eshell buffer.")
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char eshell-last-output-end)
        (beginning-of-line)
        (if (= (point-min) (point))
            (message "Nothing to clear.")
          (delete-region (point-min) (point))
          (message "Cleared."))))))

(defun fjl/eshell-ido-history ()
  "Select an eshell history item with ido and insert it."
  (interactive)
  (let ((cmd (completing-read "Eshell history: " (delete-dups (ring-elements eshell-history-ring)))))
    (delete-region eshell-last-output-end (point))
    (goto-char eshell-last-output-end)
    (insert cmd)))

(defun fjl/eshell-prompt ()
  (save-match-data
    (let* ((home (expand-file-name "~"))
           (default-directory (expand-file-name default-directory))
           (inhome (string-prefix-p home default-directory))
           (pwd (substring default-directory (if inhome (length home) 0))))
      (concat (if inhome "~" "")
              ;; todo: make more comps visible of last one is < 3 characters
              (cl-loop for dircons on (split-string pwd "/" t)
                       for dir = (car dircons)
                       for last = (null (cdr dircons))
                       for sublen = (if (= (aref dir 0) ?.) 2 1)
                       concat "/"
                       concat (if last dir (substring dir 0 sublen)))
              " >> "))))

;;;###autoload
(defun fjl/eshell-restart-command (&optional arg)
  "Interrupt and restart the current command.
If the current buffer is not an eshell, the most recently
used visible eshell is used.

With a prefix argument, it invokes the ARGth last command instead."
  (interactive "p")
  (let ((win (fjl/mru-eshell-window)))
    (if (null win)
        (message "No visible eshell buffer.")
      (with-selected-window win
        (eshell-interrupt-process)
        (sit-for 0.1)
        (eshell-previous-input (or arg 0))
        (goto-char (point-max))
        (eshell-send-input)))))

(defvar fjl/eshell-command-replacements nil
  "This variable defines additional replacements for
shell commands, similar to \"info\", \"grep\", etc.

Each entry should be of the form (command . replacement),
where command is a list of the form (name arg ...).
Replacements are only invoked when the command is used outside
of a pipeline and has no options specified.")

(defun fjl/eshell-named-command-hook (cmd args)
  (unless eshell-in-pipeline-p
    (dolist (subst fjl/eshell-command-replacements)
      (let ((want-cmd (caar subst))
            (want-args (cdar subst)))
        (when (and (string= cmd want-cmd)
                   (equal (cl-subseq args 0 (length want-args)) want-args))
          (throw 'eshell-replace-command
                 (eshell-parse-command (cdr subst) (cl-subseq args (length want-args)))))))))

(add-hook 'eshell-named-command-hook 'fjl/eshell-named-command-hook)

(defun fjl/previous-prompt (regexp n)
  (save-match-data
    (forward-line 0)
    (re-search-backward regexp (point-min) 'noerror n)
    ;; skip one prompt forward
    (let ((eol (line-end-position)))
      (if (and (looking-at regexp)
               (<= (match-end 0) eol))
          (goto-char (match-end 0))))))

(defun fjl/next-prompt (regexp n)
  (save-match-data
    (re-search-forward regexp (point-max) 'noerror n)))

(defun fjl/mru-eshell-window ()
  (or (fjl/find-mru-window-with-mode 'eshell-mode (selected-frame))
      ;; walk all frames if there is no eshell on the current frame
      (fjl/find-mru-window-with-mode 'eshell-mode 'visible)))

(defun fjl/find-mru-window-with-mode (-mode &optional all-frames)
  (let ((max-use-time 0)
        (match nil))
    (walk-windows
     (lambda (w)
       (message "win: %S mode:%S want:%S" w (buffer-local-value 'major-mode (window-buffer w)) -mode)
       (when (and (eq (buffer-local-value 'major-mode (window-buffer w)) -mode)
                  (>= (window-use-time w) max-use-time))
         (setq max-use-time (window-use-time w))
         (setq match w)))
     nil all-frames)
    match))

(defun fjl/eshell-ctrl-d (arg)
  (interactive "p")
  (if (and (= (point) (point-max)) ;; at end of buffer
           (not eshell-current-command) ;; no command is running
           (looking-back eshell-prompt-regexp (line-beginning-position))) ;; no input after prompt
      (quit-window)
    (delete-char arg)))

(defun fjl/eshell-fixup-COLUMNS ()
  "Patches the definition of COLUMNS to report less than
the actual window width. This makes output from command
adapting to terminal width not wrap."
  (dolist (el eshell-variable-aliases-list)
    (when (string= (car el) "COLUMNS")
      (rplacd el '((lambda (indices) (- (window-width) 1)) t)))))

;;;###autoload
(defun fjl/eshell-mode-hook ()
  (fjl/eshell-fixup-COLUMNS)
  (setq truncate-lines nil)
  ;; this overrides eshell-show-output, which is also bound to C-c C-r.
  (define-key eshell-mode-map (kbd "C-c C-q") (lambda () (interactive) (quit-process)))
  (define-key eshell-mode-map (kbd "C-M-l") 'fjl/eshell-clear)
  (define-key eshell-mode-map (kbd "C-M-p") 'fjl/eshell-ido-history)
  (define-key eshell-mode-map (kbd "C-d") 'fjl/eshell-ctrl-d)
  (define-key eshell-mode-map (kbd "C-c C-p")
    (lambda (n) (interactive "p") (fjl/previous-prompt eshell-prompt-regexp n)))
  (define-key eshell-mode-map (kbd "C-c C-n")
    (lambda (n) (interactive "p") (fjl/next-prompt eshell-prompt-regexp n))))

;;;###autoload
(defun fjl/term-mode-hook ()
  (setq truncate-lines nil))

;;;###autoload
(add-hook 'term-mode-hook 'fjl/term-mode-hook)
;;;###autoload
(add-hook 'eshell-mode-hook 'fjl/eshell-mode-hook)

;; From http://emacs.stackexchange.com/questions/9510/is-there-something-like-brace-expansion-in-eshell
;;
;;     >> ls "prefix-{A,B,C}.suffix"(|brexp)
(defun brexp (str)
  (let* ((parts (split-string str "[{}]"))
         (prefix (car parts))
         (body   (nth 1 parts))
         (suffix (nth 2 parts)))
    (mapcar (lambda (x) (concat prefix x suffix))
            (split-string body ","))))

(provide 'init-eshell)
