(require 'init-bootstrap)
(require 'eshell)
(require 'em-prompt)
(require 'em-hist)
(require 'ring)

;; Load 'z' extension. This tracks frequently-used directories and
;; enables jumping into them using the 'z' command.
(require 'eshell-z)

;;;###autoload
(defun fjl/eshell-hotkey (prefix-arg)
  "This function creates or selects an eshell session.
It must be invoked using a number key or function key."
  (interactive "p")
  (let* ((ev      (this-command-keys-vector))
         (lastkey (aref ev (1- (length ev))))
         (bare    (event-basic-type lastkey))
         (fkey    (cl-position bare '(f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12)))
         (numkey  (cl-position bare '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)))
         (session (1+ (or fkey numkey prefix-arg)))
         (buffer  (get-buffer-create (format "*eshell %d*" session)))
         (win     (get-buffer-window buffer 'visible)))
    (cond (win
           (select-frame-set-input-focus (window-frame win))
           (select-window win))
          ((memq 'meta (event-modifiers lastkey))
           (pop-to-buffer buffer))
          (t
           (switch-to-buffer buffer)))
    (unless (derived-mode-p 'eshell-mode)
      (eshell-mode))))

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
