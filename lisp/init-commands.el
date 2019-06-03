;; -*- lexical-binding: t -*-

(require 'init-bootstrap)

(defun init-compile ()
  "Recompile initialization files."
  (interactive)
  (make-init-autoloads nil)
  (byte-recompile-directory +fjl-init-lisp+ 0)
  (byte-recompile-directory package-user-dir 0))

(defun reinit ()
  "Recompile and reload initialization files."
  (interactive)
  (init-compile)
  (dolist (f features)
    (when (string-prefix-p "init-" (symbol-name f))
      (load (symbol-name f))))
  (load (concat +fjl-init+ "init")))

;; backported from emacs 23.4
(unless (functionp 'isearch-forward-symbol-at-point)
  (require 'thingatpt)
  (defun isearch-forward-symbol-at-point ()
    "Do incremental search forward for a symbol found near point.x
 Like ordinary incremental search except that the symbol found at point
 is added to the search string initially as a regexp surrounded
 by symbol boundary constructs \\_< and \\_>.
 See the command `isearch-forward-symbol' for more information."
    (interactive)
    (isearch-forward-symbol nil 1)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (cond
       (bounds
        (when (< (car bounds) (point))
          (goto-char (car bounds)))
        (isearch-yank-string
         (buffer-substring-no-properties (car bounds) (cdr bounds))))
       (t
        (setq isearch-error "No symbol at point")
        (isearch-update))))))

(defun fjl/join-next-line (&optional times)
  "Move content of the following line to the end of the current line.
Deletes any indentation. If point is inside a comment, also
deletes the comment start characters on the following line."
  (interactive "p")
  (save-excursion
    (dotimes (_ times)
      (forward-line 1)
      (delete-indentation)
      (when (nth 4 (syntax-ppss))
        (forward-char)
        (while (looking-at-p (regexp-quote comment-start))
          (delete-char (length comment-start)))))))

(defun rename-buffer-and-file (newname)
  "Rename the current buffer and its visited file."
  (interactive "FNew name: ")
  (let ((path (buffer-file-name)))
    (if (null path)
      (message "Buffer does not visit a file.")
      (save-buffer)
      (rename-file path newname 1) ;; number as third arg means confirm overwrite
      (set-visited-file-name newname t path))))

;; window management

;; Adapted from http://www.emacswiki.org/emacs/WindowNavigation
(defun select-mru-window ()
  "Select most-recently-used non-selected window on the current frame.
When invoked with a prefix argument, prompt for a visible buffer name."
  (interactive)
  (if (not current-prefix-arg)
      (let ((w (get-mru-window nil t t)))
        (and w (select-window w)))
    (let ((windows (remove (selected-window) (window-list nil 'not-minibuffer))))
      (when windows
        (cl-flet ((win-bufname (w) (buffer-name (window-buffer w))))
          (let* ((windows (cl-sort windows '> :key #'window-use-time))
                 (buffers (mapcar #'win-bufname windows))
                 (name (completing-read "Window: " buffers nil t nil nil (car buffers)))
                 (sel (cl-find name windows :key #'win-bufname :test #'string-equal)))
            (if sel
                (select-window sel)
              (error "Buffer '%s' has no visible window" name))))))))

(defun toggle-fullscreen (&optional frame)
  "Expand the current frame so it fills the whole screen."
  (interactive)
  (set-frame-parameter
   frame 'fullscreen
   (when (not (frame-parameter frame 'fullscreen)) 'fullboth)))

(defun toggle-dedicated ()
  "Toggle dedicated flag of current window."
  (interactive)
  (let* ((dedicated (window-dedicated-p)))
    (set-window-dedicated-p nil (not dedicated))
    (message "Window is %s dedicated to %s"
             (if dedicated "no longer" "now")
             (buffer-name))))

(defun fjl/split-window-sensibly (&optional window)
  "This is very similary to the built-in `split-window-sensibly'
but will try splitting horizontally first."
  (let ((window (or window (selected-window))))
    (if (window-dedicated-p window)
        nil ;; don't split dedicated windows, ever.
      (or
       (and (window-splittable-p window t)
            (with-selected-window window
              (split-window-right)))
       (split-window-sensibly window)))))

;; editing

(defun fjl/comment-enter (arg)
  "Inserts a newline. Also inserts comment start characters if
point is inside a comment."
  (interactive "p")
  (if (nth 4 (syntax-ppss))
      (comment-indent-new-line arg)
    (newline arg)))

(defun title-case-region-or-line (begin end)
  "Title case text between nearest brackets, or current line, or text selection.
Capitalize first letter of each word, except words like {to, of,
the, a, in, or, and, …}. If a word already contains cap letters
such as HTTP, URL, they are left as is."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (p1 p2
           (skip-chars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
       (progn
         (skip-chars-backward skip-chars (line-beginning-position))
         (setq p1 (point))
         (skip-chars-forward skip-chars (line-end-position))
         (setq p2 (point)))
       (list p1 p2))))
  (let* ((pairs [[" A " " a "]
                 [" And " " and "]
                 [" At " " at "]
                 [" As " " as "]
                 [" By " " by "]
                 [" Be " " be "]
                 [" Into " " into "]
                 [" In " " in "]
                 [" Is " " is "]
                 [" It " " it "]
                 [" For " " for "]
                 [" Of " " of "]
                 [" Or " " or "]
                 [" On " " on "]
                 [" Via " " via "]
                 [" The " " the "]
                 [" That " " that "]
                 [" To " " to "]
                 [" Vs " " vs "]
                 [" With " " with "]
                 [" From " " from "]
                 ["'S " "'s "]]))
    (save-excursion
      (save-restriction
        (narrow-to-region begin end)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda (x)
             (goto-char (point-min))
             (while
                 (search-forward (aref x 0) nil t)
               (replace-match (aref x 1) 'FIXEDCASE 'LITERAL)))
           pairs))))))

;; from http://www.emacswiki.org/emacs/VisualLineMode
(defvar visual-wrap-column nil)

(defun set-visual-wrap-column (new-wrap-column &optional buffer)
  "Force visual line wrap at NEW-WRAP-COLUMN in BUFFER (defaults
    to current buffer) by setting the right-hand margin on every
    window that displays BUFFER.  A value of NIL or 0 for
    NEW-WRAP-COLUMN disables this behavior."
  (interactive (list (read-number "New visual wrap column, 0 to disable: " (or visual-wrap-column fill-column 0))))
  (if (and (numberp new-wrap-column)
           (zerop new-wrap-column))
      (setq new-wrap-column nil))
  (with-current-buffer (or buffer (current-buffer))
    (visual-line-mode t)
    (set (make-local-variable 'visual-wrap-column) new-wrap-column)
    (add-hook 'window-configuration-change-hook 'update-visual-wrap-column nil t)
    (let ((windows (get-buffer-window-list)))
      (while windows
        (when (window-live-p (car windows))
          (with-selected-window (car windows)
            (update-visual-wrap-column)))
        (setq windows (cdr windows))))))

(defun update-visual-wrap-column ()
  (if (not visual-wrap-column)
      (progn
        (set-window-margins nil nil)
        (set-window-parameter nil 'min-margins nil))
    (let* ((current-margins (window-margins))
           (right-margin (or (cdr current-margins) 0))
           (current-width (window-width))
           (current-available (+ current-width right-margin)))
      (if (<= current-available visual-wrap-column)
          (set-window-margins nil (car current-margins))
        (set-window-margins nil (car current-margins)
                            (- current-available visual-wrap-column)))
      ;; Very recent emacs (> emacs 25.1), this parameter allows splitting the window
      ;; regardless of the huge margin.
      (set-window-parameter nil 'min-margins (cons 0 1)))))

;; search

(defun multi-isearch-glob (glob)
  "Prompt for a glob (defaults to same extension in current directory)
and launch multi-isearch."
  (interactive
   (let* ((ext (file-name-extension (buffer-file-name)))
          (default (concat "*." ext)))
     (list (read-file-name "Multi I-search glob: " nil default nil default))))
  (multi-isearch-files (file-expand-wildcards glob)))

;; run shell commands with completion
(defun launcher (&optional initial-input)
  "Launch a program in the background, completing candidates using ivy.
Optional INITIAL-INPUT is the initial input in the minibuffer."
  (interactive)
  (unless initial-input
    (setq initial-input (cdr (assoc this-command ivy-initial-inputs-alist))))
  (ivy-read "Async Shell Command: " (fjl/shell-command-completions)
            :action (lambda (cmd) (start-process-shell-command cmd nil cmd))
            :initial-input initial-input))

(defun fjl/shell-command-completions ()
  (let (result)
    (dolist (dir exec-path)
      (dolist (file (ignore-errors (directory-files dir)))
        (unless (or (string= file ".") (string= file ".."))
          (push file result))))
    (delete-dups (sort result 'string-lessp))))

;; dired

(defvar fjl/dired-afplay-process nil)

;;;###autoload
(defun dired-afplay ()
  "Plays back the audio file under cursor using afplay."
  (interactive)
  (when (and fjl/dired-afplay-process (process-live-p fjl/dired-afplay-process))
    (kill-process fjl/dired-afplay-process))
  (let ((file (expand-file-name (dired-file-name-at-point))))
    (setq fjl/dired-afplay-process (start-process "*dired afplay*" nil "afplay" file))))

(provide 'init-commands)
