;;; stickies.el --- Sticky notes in dedicated frames -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Felix Lange

;; Author: Felix Lange <fjl@twurst.com>
;; Maintainer: Felix Lange <fjl@twurst.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience
;; URL: https://github.com/fjl/stickies.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Apple Stickies-style sticky notes for Emacs.  Each note is a flat
;; file in `stickies-directory'.  The directory listing is the source
;; of truth for which notes exist; the index file holds per-note
;; metadata (theme, frame geometry).

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'face-remap)
(require 'files-x)
(require 'easymenu)

(defgroup stickies nil
  "Sticky notes in dedicated frames."
  :group 'convenience
  :prefix "stickies-")

(defcustom stickies-directory
  (expand-file-name "stickies/" "~")
  "Directory holding all sticky note files.
Every non-hidden file in this directory is treated as a sticky note.
The per-note metadata index lives here too, in a hidden file."
  :type 'directory)

(defcustom stickies-themes
  '((yellow :background "#fff8b8" :foreground "#222222" :border "#cdb94f")
    (pink   :background "#fcc9c9" :foreground "#222222" :border "#d68a8a")
    (purple :background "#d8c9f0" :foreground "#222222" :border "#a98ad1")
    (blue   :background "#c2dffc" :foreground "#222222" :border "#7fabd6")
    (green  :background "#c5edc6" :foreground "#222222" :border "#85bd86"))
  "Named color themes for sticky notes.
Each entry has the form (NAME PROPERTIES) where PROPERTIES is a plist with
`:background' and `:foreground' colors and an optional `:border' color
\(for the header line's underline and the minibuffer frame's edge).  When
`:border' is omitted it is derived by darkening `:background' (see
`stickies-border-darken')."
  :type '(alist :key-type symbol :value-type sexp))

(defcustom stickies-default-theme 'yellow
  "Name of the default theme used for new sticky notes.
Must be a key in `stickies-themes'."
  :type 'symbol)

(defvar stickies--protected-faces
  '(;; Faces handled specially or structural to a note's own rendering.
    default header-line cursor fringe
    border internal-border child-frame-border scroll-bar
    vertical-border window-divider
    window-divider-first-pixel window-divider-last-pixel
    stickies-close-button-hover stickies-roll-button-hover)
  "Faces never flattened, regardless of `stickies-flatten-exclude'.
These are system-level faces that must keep their own colors: the
note's own structural faces plus frame and window chrome.")

(defcustom stickies-flatten-exclude
  '(region secondary-selection highlight hl-line
    isearch lazy-highlight isearch-fail match
    show-paren-match show-paren-mismatch
    error warning success
    link mouse tooltip
    mode-line mode-line-inactive mode-line-buffer-id
    mode-line-emphasis mode-line-highlight
    tab-bar tab-bar-tab tab-bar-tab-inactive tab-line
    ;; Completion-UI selection and match highlighting, so the current
    ;; candidate and matched text stay visible in the minibuffer frame.
    completions-common-part completions-first-difference completions-highlight
    completions-annotations icomplete-selected-match
    ivy-current-match ivy-minibuffer-match-highlight
    ivy-minibuffer-match-face-1 ivy-minibuffer-match-face-2
    ivy-minibuffer-match-face-3 ivy-minibuffer-match-face-4
    swiper-match-face-1 swiper-match-face-2
    swiper-match-face-3 swiper-match-face-4
    vertico-current selectrum-current-candidate
    orderless-match-face-0 orderless-match-face-1
    orderless-match-face-2 orderless-match-face-3
    helm-selection helm-match)
  "Additional faces that keep their colors in sticky notes.
Includes selection and match-highlight faces of the common completion
UIs (built-in completions, ivy, swiper, vertico, selectrum, orderless,
helm) so the current candidate stays visible in a note's minibuffer
frame.  Faces not currently defined are simply ignored.

Customize this to preserve more (or fewer) faces when flattening a note
and its minibuffer frame to the theme colors.  The faces in
`stickies--protected-faces' are always excluded on top of these."
  :type '(repeat face))

(defcustom stickies-face-remaps nil
  "Extra face remaps applied after flattening.
Each entry is (FACE . SPEC), where SPEC is the property-list form
accepted by `face-remap-add-relative', e.g.
  (org-todo :foreground \"red\" :weight bold).
Applied last so they override flattening."
  :type '(alist :key-type face :value-type sexp))

(defcustom stickies-title-format '("%b")
  "Title shown in a sticky note's header line.
A mode-line construct (see `mode-line-format') rendered with
`format-mode-line', so it may hold strings, %-constructs, your own
variables, and `:eval' forms.  Evaluated in the note's buffer."
  :type 'sexp
  :risky t)

(defcustom stickies-header-text-height 0.8
  "Scale of a note's chrome text relative to its body.
Applies to the smaller text used for the header line and the minibuffer.
A value below 1.0 makes them smaller than the note body; nil leaves them
at the body's size."
  :type '(choice (const :tag "Same as body" nil) number))

(defvar stickies-frame-parameters
  '((width . 40)
    (height . 12)
    (undecorated . t)
    (drag-with-header-line . t)
    (unsplittable . t)
    (vertical-scroll-bars . nil)
    (internal-border-width . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0))
  "Default frame parameters for sticky note frames.
A `(stickies-note . BASENAME)' marker and a `minibuffer' pointing at the
note's minibuffer child frame are added automatically (see
`stickies--make-frame').  Saved per-note geometry overrides these.")

(defcustom stickies-auto-save-interval 2
  "Idle seconds before a modified sticky note is auto-saved.
Set to nil to disable auto-saving."
  :type '(choice (number :tag "Seconds")
                 (const :tag "Off" nil)))


;;;; Index state and I/O

(defvar stickies--notes nil
  "Alist of per-note metadata.
Each entry has the form (BASENAME . ATTRS) where ATTRS is an
alist that may contain `:theme' (a symbol naming an entry in
`stickies-themes'), `:params' (an alist of frame parameters), and
`:rolled-up' (whether the sticky note was last seen in the rolled-up
state).  Notes are identified by their basename within
`stickies-directory'.")

(defun stickies--index-file ()
  "Return the absolute path of the index file.
It's a dotfile inside `stickies-directory' so directory listings
of notes naturally skip it."
  (expand-file-name ".stickies-index.eld" stickies-directory))

(defun stickies--save-index ()
  "Write `stickies--notes' to the index file."
  (unless (file-directory-p stickies-directory)
    (make-directory stickies-directory t))
  (with-temp-file (stickies--index-file)
    (let ((print-length nil)
          (print-level nil))
      (insert ";; sticky note index\n")
      (prin1 stickies--notes (current-buffer))
      (insert "\n"))))

(defun stickies--load-index ()
  "Load `stickies--notes' from the index file if present.
Drops entries that no longer refer to existing files."
  (let ((file (stickies--index-file)) index)
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (setq index (read (current-buffer)))))
    (if (stickies--validate-index index)
        (progn
          (setq stickies--notes index)
          (stickies--prune-index))
      (message "Sticky note index %s is invalid. %s" file index)
      (setq stickies--notes nil))))

(defun stickies--validate-index (index)
  "Return non-nil if INDEX is a well-formed sticky note index."
  (and (listp index)
       (seq-every-p
        (lambda (entry)
          (and (listp entry)
               (stringp (car entry))
               (seq-every-p #'consp (cdr entry))))
        index)))

(defun stickies--prune-index ()
  "Drop index entries whose files no longer exist in `stickies-directory'."
  (setq stickies--notes
        (cl-remove-if-not
         (lambda (entry)
           (let ((key (car entry)))
             (and (stringp key)
                  (not (string-match-p "/" key))
                  (file-exists-p (stickies--note-path key)))))
         stickies--notes)))

(defun stickies--note-path (basename)
  "Return the absolute path for BASENAME under `stickies-directory'."
  (expand-file-name basename stickies-directory))

(defun stickies--note-file-p (basename)
  "Return non-nil if BASENAME names a real sticky note file.
Excludes hidden files (leading dot -- the index, lock files such as
`.#note.txt', etc.), Emacs backup files (trailing `~') and auto-save
files (`#note.txt#'), none of which should be opened as notes."
  (and (stringp basename)
       (not (string-empty-p basename))
       (not (string-prefix-p "." basename))
       (not (string-suffix-p "~" basename))
       (not (and (string-prefix-p "#" basename)
                 (string-suffix-p "#" basename)))))

(defun stickies--note-basename (path)
  "Return PATH's basename if it is a note file under `stickies-directory'.
Returns nil for paths outside the directory and for non-note files
\(hidden, backup or auto-save files; see `stickies--note-file-p')."
  (and path
       (file-directory-p stickies-directory)
       (let ((p (expand-file-name path)))
         (when (file-in-directory-p p stickies-directory)
           (let ((basename (file-name-nondirectory p)))
             (and (stickies--note-file-p basename) basename))))))

(defun stickies--entry (basename)
  "Return the index cell for BASENAME, or nil."
  (assoc basename stickies--notes))

(defun stickies--register (basename)
  "Ensure BASENAME has an index entry.  Return its cell."
  (let ((cell (stickies--entry basename)))
    (unless cell
      (setq cell (list basename))
      (push cell stickies--notes)
      (stickies--save-index))
    cell))

(defun stickies--unregister (basename)
  "Remove BASENAME from the index."
  (setq stickies--notes
        (cl-remove basename stickies--notes
                   :key #'car :test #'string=))
  (stickies--save-index))

(defun stickies--all-notes ()
  "Return basenames of all note files in `stickies-directory'.
Hidden, backup and auto-save files are excluded; see
`stickies--note-file-p'."
  (when (file-directory-p stickies-directory)
    (cl-remove-if-not #'stickies--note-file-p
                      (directory-files stickies-directory))))


;;;; Color application

(defun stickies--current-theme ()
  "Return the active theme symbol for the current sticky note buffer."
  (let ((basename (and buffer-file-name
                       (stickies--note-basename buffer-file-name))))
    (or (and basename
             (alist-get :theme (cdr (stickies--entry basename))))
        stickies-default-theme)))

(defun stickies--theme-colors ()
  "Return (BG . FG) for the current buffer's theme."
  (let* ((name (stickies--current-theme))
         (spec (or (cdr (assq name stickies-themes))
                   (cdr (assq stickies-default-theme stickies-themes)))))
    (cons (plist-get spec :background)
          (plist-get spec :foreground))))

(defcustom stickies-border-darken 18
  "How much darker than the theme background a derived border is, in percent.
Used only for a theme without an explicit `:border' (see `stickies-themes')."
  :type 'integer)

(defun stickies--theme-border ()
  "Return the border color for the current buffer's theme.
The theme's `:border', or `:background' darkened by `stickies-border-darken'."
  (let* ((name (stickies--current-theme))
         (spec (or (cdr (assq name stickies-themes))
                   (cdr (assq stickies-default-theme stickies-themes)))))
    (or (plist-get spec :border)
        (color-darken-name (plist-get spec :background) stickies-border-darken))))

(defun stickies--paint-frame-faces (frame bg fg border)
  "Pin every non-excluded face of FRAME to BG/FG, frame-locally.
Chrome blends into BG; the child-frame border uses BORDER.  Colors are
set absolutely (not relative as `face-remap' would) so the user's global
theme cannot show through -- both fg and bg are pinned, while
slant/weight/etc. are left alone.  Used for both a note frame and its
minibuffer child frame, so the prompt and completions match the note."
  ;; `default' (and the area painted below buffer text) via frame params
  ;; rather than the face, so it survives the face cache.
  (set-frame-parameter frame 'background-color bg)
  (set-frame-parameter frame 'foreground-color fg)
  (set-face-attribute 'header-line frame
                      :background bg
                      :foreground fg
                      :height (or stickies-header-text-height 1.0)
                      ;; A faint box all around, matching the note
                      ;; minibuffer's full border.  Positive line-width
                      ;; insets the text by 1px, just as the minibuffer's
                      ;; child-frame border insets its content -- so the
                      ;; prompt lands exactly where the header text was,
                      ;; with no 1px shift when the minibuffer appears.
                      :box `(:line-width (1 . 1) :color ,border)
                      :underline nil
                      :overline nil)
  ;; Fringe and other chrome blend into the background; the child-frame
  ;; border (the minibuffer's visible 1px edge) uses the border color.
  (dolist (face '(fringe internal-border border
                  scroll-bar vertical-border window-divider
                  window-divider-first-pixel window-divider-last-pixel))
    (when (facep face) (set-face-attribute face frame :background bg)))
  (when (facep 'child-frame-border)
    (set-face-attribute 'child-frame-border frame :background border))
  (when (facep 'cursor) (set-face-attribute 'cursor frame :background fg))
  ;; Header-line button hover: a darker background (the border color), so
  ;; it reads as a press target and meets the header's border seamlessly
  ;; instead of painting the plain note background over it.
  (dolist (face '(stickies-close-button-hover stickies-roll-button-hover))
    (when (facep face)
      (set-face-attribute face frame :background border :foreground fg :box nil)))
  ;; Pin every other face to the theme colors -- including color-less
  ;; faces such as `italic'/`bold', whose text would otherwise inherit
  ;; the frame's background and show a stray patch under a dark global
  ;; theme.  Setting only the colors preserves slant/weight/etc.
  (dolist (face (face-list))
    (unless (or (memq face stickies--protected-faces)
                (memq face stickies-flatten-exclude))
      (set-face-attribute face frame :foreground fg :background bg)))
  (pcase-dolist (`(,face . ,spec) stickies-face-remaps)
    (apply #'set-face-attribute face frame spec))
  ;; Repaint the cached fringe/border pixels the attributes above don't.
  (redraw-frame frame))

(defun stickies--apply-frame-colors (frame)
  "Paint note FRAME and its minibuffer child frame with its theme colors.
The colors are attributes of the frames, not of the note's buffer: the
same buffer shown elsewhere keeps that frame's normal faces, and nothing
displayed in either frame -- buffer text, the prompt, completions,
chrome, the empty area below the last line -- can escape the theme."
  (when (frame-live-p frame)
    (pcase-let ((`(,bg ,fg ,border)
                 (with-current-buffer (stickies--frame-buffer frame)
                   (let ((c (stickies--theme-colors)))
                     (list (car c) (cdr c) (stickies--theme-border))))))
      (stickies--paint-frame-faces frame bg fg border)
      (when-let ((mini (stickies--minibuffer-frame-of frame)))
        (stickies--paint-frame-faces mini bg fg border)))))

(defun stickies--apply-colors ()
  "Recolor the note frame(s) currently displaying the buffer.
Called from a buffer context (mode setup, theme change); the actual
painting is frame-local, see `stickies--apply-frame-colors'.  Only real
note frames are touched, so the buffer shown elsewhere stays normal."
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (let ((frame (window-frame window)))
      (when (frame-parameter frame 'stickies-note)
        (stickies--apply-frame-colors frame)))))


;;;; Header line

;; Each button gets its own mouse-face symbol so Emacs treats them as
;; separate highlight regions -- hovering one doesn't light up the other.
(defface stickies-close-button-hover '((t :inherit mode-line-highlight))
  "Mouse hover face for the sticky note close button.")
(defface stickies-roll-button-hover '((t :inherit mode-line-highlight))
  "Mouse hover face for the sticky note roll-up button.")

(defun stickies--button-close (_event)
  "Header-line button: close (delete) the current sticky note frame."
  (interactive "e")
  (delete-frame))

(defvar stickies--close-button-map
  (let ((m (make-sparse-keymap)))
    (define-key m [header-line mouse-1] #'stickies--button-close)
    m)
  "Keymap for the close button in the sticky note header line.")

(defun stickies--rolled-up-p (&optional frame)
  "Return non-nil if FRAME (default: selected) is rolled up.
The value is the pre-rolled frame height, in lines."
  (frame-parameter frame 'stickies-roll-saved-height))

(defun stickies--button-roll (_event)
  "Header-line button: toggle the roll-up state of the current sticky note."
  (interactive "e")
  (stickies-toggle-roll-up))

(defvar stickies--roll-button-map
  (let ((m (make-sparse-keymap)))
    (define-key m [header-line mouse-1] #'stickies--button-roll)
    m)
  "Keymap for the roll-up button in the sticky note header line.")

(defun stickies--header-line ()
  "Return the header-line string for the current sticky note."
  (let* ((title (format-mode-line stickies-title-format))
         (roll (propertize
                (if (stickies--rolled-up-p) " ↓ " " ↑ ")
                'mouse-face 'stickies-roll-button-hover
                'help-echo "mouse-1: roll up/down this sticky note"
                'local-map stickies--roll-button-map))
         (close (propertize
                 " x "
                 'mouse-face 'stickies-close-button-hover
                 'help-echo "mouse-1: close this sticky note"
                 'local-map stickies--close-button-map))
         (fill (propertize
                " " 'display
                `(space :align-to (- right ,(+ (length roll)
                                               (length close)))))))
    (concat " " title fill roll close)))


;;;; Context menu

(defun stickies--set-theme (name)
  "Set NAME as the theme for the current sticky note and persist it."
  (let* ((basename (file-name-nondirectory buffer-file-name))
         (cell (stickies--register basename)))
    (setf (alist-get :theme (cdr cell)) name)
    (stickies--save-index))
  (stickies--apply-colors))

(defun stickies--always-on-top-p (&optional frame)
  "Return non-nil if FRAME (defaults to selected) stays above other windows."
  (eq (frame-parameter frame 'z-group) 'above))

(defun stickies-toggle-always-on-top ()
  "Toggle whether the current sticky note frame stays above other windows."
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-parameter
     frame 'z-group (if (stickies--always-on-top-p frame) nil 'above))
    (stickies--save-frame-state frame)))

(defvar-local stickies--roll-overlay nil
  "Marker overlay set while the sticky note's buffer is in rolled-up state.")

(defun stickies--frame-buffer (frame)
  "Return the buffer shown in FRAME's root window."
  (window-buffer (frame-root-window frame)))

(defun stickies--enter-rolled-up ()
  "Hide buffer content while the sticky note is rolled up.
Keeps the real header line active so `drag-with-header-line' --
Emacs's built-in, glitch-free frame drag -- continues to work.
An invisible overlay covers the entire buffer so the (small)
body row paints as blank under the header."
  (unless stickies--roll-overlay
    (setq-local cursor-type nil)
    (let ((o (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put o 'invisible t)
      (setq stickies--roll-overlay o))))

(defun stickies--exit-rolled-up ()
  "Restore the buffer's normal display."
  (when stickies--roll-overlay
    (delete-overlay stickies--roll-overlay)
    (setq stickies--roll-overlay nil)
    (kill-local-variable 'cursor-type)))

(defun stickies--apply-roll-height (frame)
  "Shrink FRAME to the minimal height with the header line still visible.
`window_wants_header_line' in src/window.c keeps the header iff
WINDOW_PIXEL_HEIGHT > frame_char_height (no mode line).
`set-frame-height' with PIXELWISE sets the frame's text height,
which equals WINDOW_PIXEL_HEIGHT for a single-window frame -- so
passing `frame_char_height + 1' is just enough to keep the
header.  An invisible overlay added in `stickies--enter-rolled-up'
hides whatever buffer content would otherwise paint in the
resulting few-pixel body strip.  The achieved text height in
*pixels* is recorded in `stickies-roll-height' so the resize hook
can tell our own resize from an external one -- line granularity
is too coarse here, since a sub-line drag can hide the header
without changing the frame's height in lines."
  (let ((window-min-height 0)
        (window-safe-min-height 0)
        (frame-resize-pixelwise t))
    (set-frame-height frame (1+ (frame-char-height frame)) nil t))
  (set-frame-parameter frame 'stickies-roll-height
                       (frame-text-height frame)))

(defun stickies-toggle-roll-up ()
  "Toggle whether the current sticky note frame is rolled up.
When rolled up the body shrinks to one natural row -- the
smallest size at which Emacs reliably keeps the header line
visible, so `drag-with-header-line' continues to move the frame
natively.  A rolled-up frame has a fixed height: attempts to
resize it vertically are undone, while width changes are kept."
  (interactive)
  (let ((frame (selected-frame)))
    (if-let ((saved (stickies--rolled-up-p frame)))
        ;; Expand.
        (progn
          (set-frame-parameter frame 'stickies-roll-saved-height nil)
          (set-frame-parameter frame 'stickies-roll-height nil)
          (with-current-buffer (stickies--frame-buffer frame)
            (stickies--exit-rolled-up))
          (set-frame-height frame saved))
      ;; Roll up.
      (set-frame-parameter frame 'stickies-roll-saved-height
                           (frame-parameter frame 'height))
      (with-current-buffer (stickies--frame-buffer frame)
        (stickies--enter-rolled-up))
      (stickies--apply-roll-height frame))
    (stickies--save-frame-state frame)))

(defun stickies--keep-roll-height-on-resize (frame)
  "Undo vertical resizing of a rolled-up sticky note FRAME.
A rolled-up sticky note has a fixed height; any resize that changes
its height is reverted to the rolled-up height, while width
changes are left intact.  The `set-frame-height' inside
`stickies--apply-roll-height' re-enters this hook, but the height
then matches `stickies-roll-height' so the guard stops the
recursion."
  (when (and (frame-parameter frame 'stickies-note)
             (stickies--rolled-up-p frame)
             (not (equal (frame-text-height frame)
                         (frame-parameter frame 'stickies-roll-height))))
    (stickies--apply-roll-height frame)))

(add-hook 'window-size-change-functions #'stickies--keep-roll-height-on-resize)

(defun stickies--save-all-frame-state ()
  "Persist geometry of every visible sticky note frame."
  (dolist (frame (stickies--frames))
    (stickies--save-frame-state frame)))

(add-hook 'kill-emacs-hook #'stickies--save-all-frame-state)

(defun stickies--popup-menu (event)
  "Show the sticky note context menu at EVENT location."
  (interactive "e")
  (let* ((current (stickies--current-theme))
         (theme-items
          (mapcar (lambda (entry)
                    (let ((name (car entry)))
                      `[,(capitalize (symbol-name name))
                        (stickies--set-theme ',name)
                        :style radio
                        :selected ,(eq name current)]))
                  stickies-themes))
         (menu (easy-menu-create-menu
                "Sticky note"
                (append '(["New Note" stickies-new]
                          ["Rename..." stickies-rename]
                          ["Delete..." stickies-delete]
                          "--")
                        theme-items
                        '("--"
                          ["Always on top" stickies-toggle-always-on-top
                           :style toggle
                           :selected (stickies--always-on-top-p)]
                          ["Rolled up" stickies-toggle-roll-up
                           :style toggle
                           :selected (stickies--rolled-up-p)]
                          "--"
                          ["Close note" delete-frame])))))
    (popup-menu menu event)))

(defvar stickies-note-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m [mouse-3] #'stickies--popup-menu)
    (define-key m [header-line mouse-3] #'stickies--popup-menu)
    m)
  "Keymap for `stickies-note-mode'.")


;;;; Auto-save

(defvar stickies-note-mode)             ; defined below via `define-minor-mode'

(defvar stickies--auto-save-timer nil
  "Idle timer that saves modified sticky note buffers.")

(defun stickies--auto-save-tick ()
  "Save modified sticky note buffers and stale frame geometries.
Runs on the same idle timer so position/size changes (which have
no dedicated hook) get persisted within one tick interval without
writing the index on every pixel of drag."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and stickies-note-mode
                 buffer-file-name
                 (buffer-modified-p))
        (let ((save-silently t))
          (save-buffer)))))
  (stickies--save-stale-frame-state))

(defun stickies--save-stale-frame-state ()
  "Persist geometry of any sticky note frame whose state changed since last save.
Writes the index file at most once even when several frames are dirty."
  (let (dirty)
    (dolist (frame (stickies--frames))
      (let ((basename (frame-parameter frame 'stickies-note)))
        (when basename
          (let ((cell (stickies--entry basename)))
            (when cell
              (let ((cur-params (stickies--frame-geometry frame))
                    (cur-roll   (and (stickies--rolled-up-p frame) t))
                    (cur-scale  (with-current-buffer (stickies--frame-buffer frame)
                                  (if (boundp 'text-scale-mode-amount)
                                      text-scale-mode-amount
                                    0)))
                    (saved-params (alist-get :params (cdr cell)))
                    (saved-roll   (alist-get :rolled-up (cdr cell)))
                    (saved-scale  (or (alist-get :text-scale (cdr cell)) 0)))
                (unless (equal cur-params saved-params)
                  (setf (alist-get :params (cdr cell)) cur-params)
                  (setq dirty t))
                (unless (eq cur-roll saved-roll)
                  (setf (alist-get :rolled-up (cdr cell)) cur-roll)
                  (setq dirty t))
                (unless (eql cur-scale saved-scale)
                  (setf (alist-get :text-scale (cdr cell)) cur-scale)
                  (setq dirty t))))))))
    (when dirty
      (stickies--save-index))))

(defun stickies--ensure-auto-save-timer ()
  "Start the auto-save idle timer if it isn't already running."
  (when (and (numberp stickies-auto-save-interval)
             (not (memq stickies--auto-save-timer timer-idle-list)))
    (setq stickies--auto-save-timer
          (run-with-idle-timer stickies-auto-save-interval t
                               #'stickies--auto-save-tick))))


;;;; Text scale

(defun stickies--apply-text-scale ()
  "Apply the current sticky note buffer's stored text-scale, if any.
The amount is read from the buffer's index entry (see
`stickies--save-stale-frame-state', which persists it)."
  (when-let* ((basename (and buffer-file-name
                             (stickies--note-basename buffer-file-name)))
              (amount (alist-get :text-scale (cdr (stickies--entry basename)))))
    (text-scale-set amount)))


;;;; Minor mode

;;;###autoload
(define-minor-mode stickies-note-mode
  "Minor mode for buffers that are sticky notes.
Applies the buffer's theme colors via a `default' face remap,
hides the mode line, installs a header line with a close button,
binds `mouse-3' to a context menu for changing themes, and closes
the corresponding sticky note frame when the buffer is killed."
  :lighter " Stk"
  :keymap stickies-note-mode-map
  (if stickies-note-mode
      (progn
        (setq-local mode-line-format nil)
        (setq-local header-line-format '(:eval (stickies--header-line)))
        ;; Hide the cursor in idle (unfocused) note frames; it reappears
        ;; in whichever note currently has focus.
        (setq-local cursor-in-non-selected-windows nil)
        (setq-local truncate-lines nil)
        (stickies--apply-colors)
        (stickies--apply-text-scale)
        (stickies--ensure-auto-save-timer)
        (add-hook 'kill-buffer-hook #'stickies--on-buffer-killed nil t))
    (kill-local-variable 'mode-line-format)
    (kill-local-variable 'header-line-format)
    (kill-local-variable 'cursor-in-non-selected-windows)
    (kill-local-variable 'truncate-lines)
    (stickies--exit-rolled-up)
    (remove-hook 'kill-buffer-hook #'stickies--on-buffer-killed t)))


;;;; Frame management

(defun stickies--frames (&optional basename)
  "Return sticky note frames, optionally filtered to BASENAME."
  (cl-loop for f in (frame-list)
           for b = (frame-parameter f 'stickies-note)
           when (and b (or (null basename) (string= b basename)))
           collect f))

(defun stickies--frame-geometry (frame)
  "Return an alist of frame parameters describing FRAME's persistent state.
Captures geometry plus toggles like `z-group'.
If FRAME is currently rolled up, save the pre-rolled (expanded)
height so a restored frame doesn't come back as a tiny strip."
  `((width   . ,(frame-parameter frame 'width))
    (height  . ,(or (stickies--rolled-up-p frame)
                    (frame-parameter frame 'height)))
    (left    . ,(frame-parameter frame 'left))
    (top     . ,(frame-parameter frame 'top))
    (z-group . ,(frame-parameter frame 'z-group))))

(defun stickies--monitor-workareas (frame)
  "Return the work areas (X Y W H) of every monitor on FRAME's display."
  (delq nil (mapcar (lambda (m) (cdr (assq 'workarea m)))
                    (display-monitor-attributes-list frame))))

(defun stickies--rect-overlap (l top r b area)
  "Return the overlap area of rectangle L TOP R B with monitor AREA.
AREA is a work area of the form (X Y W H)."
  (pcase-let ((`(,x ,y ,w ,h) area))
    (* (max 0 (- (min r (+ x w)) (max l x)))
       (max 0 (- (min b (+ y h)) (max top y))))))

(defun stickies--clamp-frame-onscreen (frame)
  "Move FRAME so it lies within a currently visible monitor work area.
Saved geometry can point at a monitor that is no longer attached;
without this a restored sticky note comes back off-screen and
unreachable.  Clamp to the monitor FRAME most overlaps, or the
first (primary) one when it overlaps none."
  (when (display-graphic-p frame)
    (pcase-let* ((`(,fl ,ft ,fr ,fb) (frame-edges frame 'outer-edges))
                 (fw (- fr fl))
                 (fh (- fb ft))
                 (areas (stickies--monitor-workareas frame)))
      (when areas
        (let ((best (car areas))
              (best-ov (stickies--rect-overlap fl ft fr fb (car areas))))
          (dolist (a (cdr areas))
            (let ((ov (stickies--rect-overlap fl ft fr fb a)))
              (when (> ov best-ov) (setq best a best-ov ov))))
          (pcase-let* ((`(,x ,y ,w ,h) best)
                       ;; Clamp so the whole frame stays inside the work
                       ;; area; pin to the corner if it is larger than the
                       ;; monitor.
                       (left (max x (min (- (+ x w) fw) fl)))
                       (top  (max y (min (- (+ y h) fh) ft))))
            (unless (and (= left fl) (= top ft))
              (set-frame-position frame left top))))))))

(defun stickies--roll-up-on-open (frame &optional attempts)
  "Roll up a newly created FRAME once the window manager honors the size.
A resize issued right after `make-frame' is rounded up to a whole
character row, because the WM has not finished placing the frame
-- leaving two text lines instead of just the header, and locking
that height in as `stickies-roll-height'.  Re-apply the rolled-up
height on short timers until the achieved text height reaches the
target or ATTEMPTS (default 20) is exhausted."
  (let ((attempts (or attempts 20)))
    (when (frame-live-p frame)
      (with-selected-frame frame
        (if (stickies--rolled-up-p frame)
            (stickies--apply-roll-height frame)
          (stickies-toggle-roll-up)))
      (when (and (> attempts 0)
                 (> (frame-text-height frame)
                    (1+ (frame-char-height frame))))
        (run-with-timer 0.05 nil
                        #'stickies--roll-up-on-open frame (1- attempts))))))

(defun stickies--make-frame (basename)
  "Create and return a frame displaying the sticky note BASENAME."
  ;; Let-bind across `make-frame': X size hints (width_inc/height_inc)
  ;; sent to the WM at frame creation depend on this variable, and the
  ;; WM uses them to round later resize requests.  Without pixel-precise
  ;; hints, our rolled-up resize would get rounded up to a whole
  ;; character row -- two text lines instead of just the header.
  (let* ((frame-resize-pixelwise t)
         (path (stickies--note-path basename))
         (entry (stickies--register basename))
         (saved (alist-get :params (cdr entry)))
         (rolled-up (alist-get :rolled-up (cdr entry)))
         ;; The note's minibuffer child frame -- created first so the note
         ;; can point its `minibuffer' parameter at its window.
         (mini-frame (stickies--make-minibuffer-frame))
         ;; Order: per-note geometry first, then user defaults, then
         ;; required markers last (so they always win).
         (params (append `((stickies-note . ,basename)
                           (name . ,(format "Sticky note: %s" basename))
                           (minibuffer . ,(minibuffer-window mini-frame)))
                         saved
                         stickies-frame-parameters))
         (buffer (find-file-noselect path))
         (frame  (make-frame params))
         (window (frame-root-window frame)))
    (set-window-buffer window buffer)
    (set-window-dedicated-p window t)
    ;; Cross-link the note and its minibuffer frame, and make the latter a
    ;; child of the note.
    (set-frame-parameter frame 'stickies-minibuffer-frame mini-frame)
    (set-frame-parameter mini-frame 'stickies-minibuffer frame)
    (set-frame-parameter mini-frame 'parent-frame frame)
    (stickies--apply-frame-colors frame)
    ;; Position the minibuffer frame now (after the note's own creation
    ;; hooks have run), so even the first echo message displays correctly
    ;; without waiting for the first interactive read.
    (stickies--position-minibuffer-frame mini-frame frame)
    ;; Restored geometry may point at a now-detached monitor; pull the
    ;; frame back onto a visible screen so it stays reachable.
    (stickies--clamp-frame-onscreen frame)
    (when rolled-up
      ;; Defer the roll-up: a synchronous resize inside `make-frame'
      ;; lands before the WM has finished sizing the new frame and gets
      ;; rounded up to two character rows.  `stickies--roll-up-on-open'
      ;; keeps re-applying the rolled-up height until the WM honors the
      ;; pixel-precise request.
      (run-with-timer 0 nil #'stickies--roll-up-on-open frame))
    frame))

(defun stickies--save-frame-state (frame)
  "Persist FRAME's geometry and rolled-up state into the index."
  (let ((basename (frame-parameter frame 'stickies-note)))
    (when basename
      (let ((cell (stickies--register basename)))
        (setf (alist-get :params (cdr cell))
              (stickies--frame-geometry frame))
        (setf (alist-get :rolled-up (cdr cell))
              (and (stickies--rolled-up-p frame) t))
        (stickies--save-index)))))

(defun stickies--on-frame-deleted (frame)
  "Persist geometry and delete the minibuffer child frame for note FRAME."
  (when (frame-parameter frame 'stickies-note)
    (stickies--save-frame-state frame)
    ;; Delete the minibuffer frame after the note is gone: it is the note's
    ;; minibuffer, so it can't be deleted while the note lives.
    (let ((mini (frame-parameter frame 'stickies-minibuffer-frame)))
      (when (and (frame-live-p mini) (not (eq mini frame)))
        (run-with-timer 0 nil
                        (lambda ()
                          (when (frame-live-p mini)
                            (ignore-errors (delete-frame mini t)))))))))

(add-hook 'delete-frame-functions #'stickies--on-frame-deleted)

;; Each sticky note has its own minibuffer-only child frame, parented to
;; it and set as its `minibuffer'.  It is hidden when idle and shown over
;; the note's content during a read or isearch, so reads, completion and
;; isearch all happen there natively while the note shows nothing when
;; idle.

(defvar minibuffer-prompt-properties)

(defcustom stickies-minibuffer-resize t
  "Whether a note's minibuffer frame grows to fit its content.
When non-nil it grows downward over the note to fit the prompt and any
completion list (so e.g. Ivy's candidates stay visible), bounded by the
note's height.  Set to nil to keep it a single line; taller content then
scrolls within it."
  :type 'boolean)

(defcustom stickies-minibuffer-max-lines nil
  "Maximum height, in lines, of a note's minibuffer frame.
nil means grow to fit the content, bounded only by the note's height --
keeping a completion UI's current candidate visible.  An integer caps it
further (more compact, but a long list may then be clipped)."
  :type '(choice (const :tag "As tall as the note" nil) integer))


(defvar stickies-minibuffer-frame-parameters
  '((minibuffer . only)
    (name . "stickies-minibuffer")
    (undecorated . t)
    (minibuffer-exit . t)               ; hide it when the minibuffer exits
    (left-fringe . 0)
    (right-fringe . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (child-frame-border-width . 0)
    (internal-border-width . 0)
    (desktop-dont-save . t)
    (no-other-frame . t)
    ;; Don't take focus when mapped: it would steal it from the note and
    ;; (for isearch, whose keys are read in the note) end isearch at once.
    (no-focus-on-map . t)
    ;; Follow the note's width/left when it is resized (not its height).
    (keep-ratio width-only . left-only)
    (visibility . nil))
  "Frame parameters for a note's minibuffer child frame.
No `z-group': it already stacks above its parent, and `z-group above'
makes it a free-floating draggable panel on macOS.")

(defun stickies--make-minibuffer-frame ()
  "Create and return a hidden minibuffer-only child frame for a note."
  (let ((after-make-frame-functions nil))
    (make-frame stickies-minibuffer-frame-parameters)))

(defun stickies--minibuffer-frame-of (frame)
  "Return the live minibuffer child frame for sticky note FRAME, or nil."
  (let ((mini (frame-parameter frame 'stickies-minibuffer-frame)))
    (and (frame-live-p mini) mini)))

(defun stickies--minibuffer-error-function (data context caller)
  "Display a command error DATA in a note's minibuffer.
Like `minibuffer-error-function' but without its `discard-input', which
busy-loops Emacs at 100% CPU in a minibuffer-only child frame (e.g. when
moving past the start of history).  CONTEXT and CALLER are as for
`command-error-default-function'."
  (if (memq 'minibuffer-quit (get (car data) 'error-conditions))
      (ding t)
    (ding))
  (let ((string (error-message-string data)))
    (let ((inhibit-message t))
      (message "%s%s" (if caller (format "%s: " caller) "") string))
    (minibuffer-message (apply #'propertize (format " [%s%s]" context string)
                               minibuffer-prompt-properties))))

(defun stickies--position-minibuffer-frame (mini note)
  "Position and theme MINI over NOTE's content area (without showing it).
Idempotent, so it can run on every echo display to undo a config that
re-applies frame parameters (e.g. a thick `child-frame-border-width') to
all frames behind our back."
  (when (and (frame-live-p mini) (frame-live-p note))
    (pcase-let* ((`(,bg ,fg ,border)
                  (with-current-buffer (stickies--frame-buffer note)
                    (let ((c (stickies--theme-colors)))
                      (list (car c) (cdr c) (stickies--theme-border)))))
                 (fringes (window-fringes (frame-root-window note)))
                 (left-fringe (or (nth 0 fringes) 0))
                 (right-fringe (or (nth 1 fringes) 0))
                 (native (frame-edges note 'native-edges))
                 (inner (frame-edges note 'inner-edges)))
      ;; Scale the frame's font so the prompt -- and everything else shown
      ;; here (completion, isearch echo, messages) -- matches the note's
      ;; header line, and the frame's line height matches so the line count
      ;; is exact.
      (let ((h (face-attribute 'default :height note)))
        (when (and stickies-header-text-height (integerp h))
          (set-face-attribute 'default mini :height
                              (round (* stickies-header-text-height h)))))
      ;; Anchor to the note's content corner, matching its fringes, with a
      ;; faint 1px border.  Child-frame LEFT/TOP are relative to NOTE's
      ;; native origin; the text width drops the fringes and border so the
      ;; frame's outer edges meet the note's content edges (pixelwise, so
      ;; the width isn't rounded to whole columns).
      (modify-frame-parameters
       mini
       `((frame-resize-pixelwise . t)
         (left . ,(- (nth 0 inner) (nth 0 native)))
         (top . ,(- (nth 1 inner) (nth 1 native)))
         (width text-pixels . ,(- (nth 2 inner) (nth 0 inner) left-fringe right-fringe 2))
         (height . 1)
         (child-frame-border-width . 1)
         (internal-border-width . 0)
         (left-fringe . ,left-fringe)
         (right-fringe . ,right-fringe)
         (background-color . ,bg)
         (foreground-color . ,fg)))
      (set-face-attribute 'fringe mini :background bg)
      (when (facep 'child-frame-border)
        (set-face-attribute 'child-frame-border mini :background border)))))

(defun stickies--show-minibuffer-frame (mini note &optional no-focus)
  "Position MINI over NOTE's content area, theme it, and show it.
With NO-FOCUS non-nil leave input focus on NOTE (for isearch, whose keys
are read in the note rather than the minibuffer)."
  (when (and (frame-live-p mini) (frame-live-p note))
    (stickies--position-minibuffer-frame mini note)
    (make-frame-visible mini)
    (unless no-focus
      (redirect-frame-focus note mini))))

(defun stickies--minibuffer-setup ()
  "Show a note's minibuffer frame over it during a minibuffer read.
On `minibuffer-setup-hook'.  A rolled-up note has no room, so roll it
down first and remember to roll it back up on exit."
  (let* ((mini (window-frame (selected-window)))
         (note (frame-parameter mini 'stickies-minibuffer)))
    (when (frame-live-p note)
      (when (stickies--rolled-up-p note)
        (set-frame-parameter note 'stickies-mini-rolled-down t)
        (with-selected-frame note (stickies-toggle-roll-up)))
      ;; Error handler that doesn't `discard-input' (see above).
      (setq-local command-error-function #'stickies--minibuffer-error-function)
      (stickies--show-minibuffer-frame mini note))))

(defun stickies--minibuffer-exit ()
  "Roll a note back up if `stickies--minibuffer-setup' rolled it down.
On `minibuffer-exit-hook'."
  (let* ((mini (window-frame (selected-window)))
         (note (frame-parameter mini 'stickies-minibuffer)))
    (when (and (frame-live-p note)
               (frame-parameter note 'stickies-mini-rolled-down))
      (set-frame-parameter note 'stickies-mini-rolled-down nil)
      (with-selected-frame note (stickies-toggle-roll-up)))))

(defun stickies--isearch-show ()
  "Show the note's minibuffer frame for isearch's echo.
On `isearch-mode-hook' (isearch uses the echo area, not a recursive
minibuffer, so `minibuffer-exit' doesn't cover it).  Keep focus on the
note so isearch reads its keys."
  (when-let ((mini (stickies--minibuffer-frame-of (selected-frame))))
    (stickies--show-minibuffer-frame mini (selected-frame) t)))

(defun stickies--note-in-isearch-p (mini)
  "Non-nil if MINI's note is currently in isearch."
  (let ((note (frame-parameter mini 'stickies-minibuffer)))
    (and (frame-live-p note)
         (buffer-local-value 'isearch-mode (stickies--frame-buffer note)))))

(defun stickies--hide-minibuffer-frames (&rest _)
  "Hide note minibuffer frames that have nothing to read.
Takes down a frame left up by a stray echo message (which `minibuffer-exit'
does not catch).  A no-op while any read is active -- gated on
`minibuffer-depth', which is stable mid-read (unlike
`active-minibuffer-window'), so it never hides a frame being read in --
and it leaves a frame alone while its note is in isearch."
  (when (zerop (minibuffer-depth))
    (dolist (f (frame-list))
      (when (and (frame-parameter f 'stickies-minibuffer)
                 (frame-visible-p f)
                 (not (stickies--note-in-isearch-p f)))
        (make-frame-invisible f)))))

(defvar stickies--saved-resize-mini-frames 'unset
  "Value of `resize-mini-frames' from before stickies took it over.")

(defun stickies--minibuffer-frame-target-height (frame)
  "Lines needed to show FRAME's minibuffer content.
Bounded by the note's height (so a completion list's current candidate
stays visible), and by `stickies-minibuffer-max-lines' if set."
  (let* ((win (frame-root-window frame))
         (char-h (frame-char-height frame))
         (pixel-h (cdr (window-text-pixel-size win nil nil nil nil)))
         (needed (max 1 (ceiling pixel-h char-h)))
         (parent (or (frame-parameter frame 'parent-frame) frame))
         (note-max (max 1 (1- (floor (frame-pixel-height parent) char-h)))))
    (min needed (if stickies-minibuffer-max-lines
                    (min stickies-minibuffer-max-lines note-max)
                  note-max))))

(defun stickies--resize-mini-frames (frame)
  "Resize a note minibuffer FRAME to fit its content; defer otherwise.
Installed as `resize-mini-frames'.  Grows for a completion list and
shrinks back for a short message, using a height computed directly from
the content rather than the function `fit-frame-to-buffer' (whose
wrapping interplay can spin redisplay on macOS).  Other minibuffer
frames keep whatever
`resize-mini-frames' did before."
  (if (frame-parameter frame 'stickies-minibuffer)
      (when stickies-minibuffer-resize
        (let ((target (stickies--minibuffer-frame-target-height frame)))
          ;; Only on a real change -- a no-op resize re-enters redisplay.
          (unless (= target (frame-height frame))
            (set-frame-height frame target))))
    (let ((prev stickies--saved-resize-mini-frames))
      (cond ((functionp prev) (funcall prev frame))
            ((and prev (not (eq prev 'unset))) (fit-frame-to-buffer frame))))))

(defvar stickies--in-set-message nil
  "Non-nil while inside `stickies--set-message-function', to avoid re-entry.")

(defun stickies--set-message-function (_message)
  "Position a note's minibuffer frame before an echo message maps it.
On the variable `set-message-functions'.  A plain message goes through neither
`minibuffer-setup-hook' nor isearch's hook, so the note's minibuffer
child frame would otherwise appear with whatever parameters were last
set on it -- which a config that re-applies `default-frame-alist' to all
frames may have clobbered.  Always returns nil, so the message itself is
passed on to the rest of the list and displayed as usual.

Only acts on a plain message (no interactive read -- `minibuffer-depth'
zero): during a read, setup and `resize-mini-frames' already manage the
frame, and this runs after Emacs has already mapped it."
  (unless stickies--in-set-message
    (let* ((stickies--in-set-message t)
           (frame (selected-frame))
           (mini (and (zerop (minibuffer-depth))
                      (frame-parameter frame 'stickies-note)
                      (stickies--minibuffer-frame-of frame))))
      (when mini
        (stickies--position-minibuffer-frame mini frame))))
  nil)

(defvar stickies--prev-clear-message-function nil
  "Value of `clear-message-function' from before stickies chained onto it.")

(defun stickies--clear-message-function ()
  "Hide note minibuffer frames when the echo area is cleared.
On `clear-message-function', which fires when the next input event
arrives -- the precise moment a stray echo message goes away.  An active
echo-area prompt (`y-or-n-p', `query-replace') is NOT cleared during its
`read-event' wait, so its frame stays up, unlike with an idle timer that
would take it down mid-prompt.  Chains to the previous function so the
echo area is still cleared as usual."
  (stickies--hide-minibuffer-frames)
  (when (functionp stickies--prev-clear-message-function)
    (funcall stickies--prev-clear-message-function)))

;; Appended, so it runs after `minibuffer-error-initialize' and wins when
;; installing `command-error-function'.
(add-hook 'minibuffer-setup-hook #'stickies--minibuffer-setup t)
(add-hook 'minibuffer-exit-hook #'stickies--minibuffer-exit)
(add-hook 'isearch-mode-hook #'stickies--isearch-show)
(add-hook 'isearch-mode-end-hook #'stickies--hide-minibuffer-frames)
(add-hook 'set-message-functions #'stickies--set-message-function)

(unless (eq clear-message-function #'stickies--clear-message-function)
  (setq stickies--prev-clear-message-function clear-message-function
        clear-message-function #'stickies--clear-message-function))

(when (eq stickies--saved-resize-mini-frames 'unset)
  (setq stickies--saved-resize-mini-frames resize-mini-frames))
(setq resize-mini-frames #'stickies--resize-mini-frames)

(defun stickies--on-buffer-killed ()
  "Close any sticky note frames showing this buffer."
  (let ((basename (stickies--note-basename buffer-file-name)))
    (when basename
      (dolist (frame (stickies--frames basename))
        (ignore-errors (delete-frame frame))))))

(defun stickies--maybe-enable ()
  "Enable `stickies-note-mode' for note files under `stickies-directory'.
Only real notes qualify; `stickies--note-basename' already rejects
hidden, backup and auto-save files, so visiting e.g. the index file
leaves the mode off."
  (when (and buffer-file-name
             (stickies--note-basename buffer-file-name)
             (not stickies-note-mode))
    (stickies-note-mode 1)))

(add-hook 'find-file-hook #'stickies--maybe-enable)
(add-hook 'after-change-major-mode-hook #'stickies--maybe-enable)


;;;; Showing notes by raising their frame

;; A sticky note's buffer lives in a dedicated frame.  Whenever something
;; tries to show that buffer -- `find-file', `switch-to-buffer',
;; `display-buffer' -- route it to the note's own frame instead of the
;; current window, creating that frame if the note doesn't have one yet.

(defvar stickies--opening-frame nil
  "Non-nil while a note frame is being created.
Guards `stickies--show-note-frame' against re-entering itself (via a
nested display of the note buffer) while `stickies--make-frame' runs.")

(defun stickies--buffer-note-basename (buffer)
  "Return the note basename BUFFER visits, or nil if it isn't a note."
  (when (buffer-live-p buffer)
    (when-let ((file (buffer-file-name buffer)))
      (stickies--note-basename file))))

(defun stickies--show-note-frame (buffer)
  "Reveal sticky note BUFFER in its own frame, creating the frame if needed.
Return the frame, or nil if BUFFER is not a note (or a frame is already
being created).  An existing frame is raised and focused; a note with no
live frame gets a fresh one."
  (unless stickies--opening-frame
    (when-let ((basename (stickies--buffer-note-basename buffer)))
      (let ((frame (car (stickies--frames basename))))
        (unless frame
          (let ((stickies--opening-frame t))
            (stickies--load-index)
            (setq frame (stickies--make-frame basename))))
        (make-frame-visible frame)
        (raise-frame frame)
        (select-frame-set-input-focus frame)
        frame))))

(defun stickies--note-buffer-name-p (buffer-name &optional _action)
  "Return non-nil if BUFFER-NAME names a sticky note buffer."
  (and (stickies--buffer-note-basename (get-buffer buffer-name)) t))

(defun stickies--display-buffer-note-frame (buffer _alist)
  "`display-buffer' action: show sticky-note BUFFER in its own frame.
Return the frame's selected window, or nil to fall through to the
default display."
  (when-let ((frame (stickies--show-note-frame buffer)))
    (frame-selected-window frame)))

(defun stickies--switch-to-buffer-advice (orig buffer-or-name &rest args)
  "Around `switch-to-buffer': show a sticky note in its own frame.
When BUFFER-OR-NAME names a sticky note, reveal that note's frame
\(creating it if needed) instead of showing the note in the current
window.  ORIG is the wrapped `switch-to-buffer' and ARGS its remaining
arguments, called unchanged otherwise."
  (or (and buffer-or-name
           (let ((buffer (get-buffer buffer-or-name)))
             (and (stickies--show-note-frame buffer) buffer)))
      (apply orig buffer-or-name args)))

(advice-add 'switch-to-buffer :around #'stickies--switch-to-buffer-advice)

(add-to-list 'display-buffer-alist
             '(stickies--note-buffer-name-p stickies--display-buffer-note-frame))


;;;; Interactive commands

(defun stickies--next-basename ()
  "Return a fresh `note-NNN.txt' whose `note-NNN' stem is unused.
The stem must be unique across all extensions so `note-001.txt'
isn't picked when `note-001.org' already exists."
  (let ((used (mapcar #'file-name-sans-extension (stickies--all-notes)))
        (n 1))
    (while (member (format "note-%03d" n) used)
      (cl-incf n))
    (format "note-%03d.txt" n)))

;;;###autoload
(defun stickies-new ()
  "Create a new sticky note in `stickies-directory' and open it.
The filename is chosen automatically; use `stickies-rename' to
rename it afterwards."
  (interactive)
  (unless (file-directory-p stickies-directory)
    (make-directory stickies-directory t))
  (stickies--load-index)
  (let* ((basename (stickies--next-basename))
         (path (stickies--note-path basename))
         (cell (stickies--register basename)))
    (with-temp-file path)
    (setf (alist-get :theme (cdr cell)) stickies-default-theme)
    (stickies--save-index)
    (stickies--make-frame basename)))

;;;###autoload
(defun stickies-open (basename)
  "Open the sticky note BASENAME from `stickies-directory'."
  (interactive
   (list (completing-read "Sticky note: " (stickies--all-notes) nil t)))
  (let ((existing (stickies--frames basename)))
    (if existing
        (progn (make-frame-visible (car existing))
               (select-frame-set-input-focus (car existing)))
      (stickies--load-index)
      (stickies--make-frame basename))))

;;;###autoload
(defun stickies-show-all ()
  "Show every sticky note in `stickies-directory'."
  (interactive)
  (stickies--load-index)
  (dolist (basename (stickies--all-notes))
    (let ((frames (stickies--frames basename)))
      (if frames
          (dolist (f frames) (make-frame-visible f))
        (stickies--make-frame basename)))))

;;;###autoload
(defun stickies-hide-all ()
  "Hide every visible sticky note frame."
  (interactive)
  (dolist (frame (stickies--frames))
    (stickies--save-frame-state frame)
    (make-frame-invisible frame t)))

;;;###autoload
(defun stickies-toggle ()
  "Toggle the visibility of all sticky notes.
If the current frame is itself a sticky note, hide every sticky note.
Otherwise show every sticky note in `stickies-directory' and raise the
frames.  Dispatching on the current frame instead of overall
visibility means one invocation from a non-sticky note frame always
brings the sticky notes forward, even when some are merely occluded
by other windows -- something Emacs has no API to detect."
  (interactive)
  (if (frame-parameter (selected-frame) 'stickies-note)
      (stickies-hide-all)
    (stickies--load-index)
    (dolist (basename (stickies--all-notes))
      (let ((frames (stickies--frames basename)))
        (if frames
            (dolist (f frames)
              (make-frame-visible f)
              (raise-frame f))
          (raise-frame (stickies--make-frame basename)))))))

;;;###autoload
(defun stickies-set-theme (name)
  "Set NAME as the theme for the current sticky note."
  (interactive
   (list (intern
          (completing-read
           "Theme: "
           (mapcar (lambda (e) (symbol-name (car e))) stickies-themes)
           nil t))))
  (unless stickies-note-mode
    (user-error "Not in a sticky note buffer"))
  (unless (assq name stickies-themes)
    (user-error "Unknown theme: %s" name))
  (stickies--set-theme name))

;;;###autoload
(defun stickies-rename (new-basename)
  "Rename the current sticky note to NEW-BASENAME (within `stickies-directory')."
  (interactive
   (progn
     (unless stickies-note-mode
       (user-error "Not in a sticky note buffer"))
     (list (read-string
            "New name: " (file-name-nondirectory buffer-file-name)))))
  (when (string-match-p "/" new-basename)
    (user-error "Name must not contain `/'"))
  (let* ((old-basename (file-name-nondirectory buffer-file-name))
         (new-path (stickies--note-path new-basename)))
    (when (string= old-basename new-basename)
      (user-error "Same name; no rename needed"))
    (when (file-exists-p new-path)
      (user-error "File already exists: %s" new-basename))
    ;; Update the index before changing the file.
    (let ((cell (stickies--entry old-basename)))
      (when cell
        (setf (car cell) new-basename)))
    (rename-visited-file new-path)
    (rename-buffer new-basename t)
    (dolist (frame (frame-list))
      (when (equal (frame-parameter frame 'stickies-note) old-basename)
        (set-frame-parameter frame 'stickies-note new-basename)
        (set-frame-parameter frame 'name (format "Sticky note: %s" new-basename))))
    (stickies--save-index)
    ;; Apply colors from the index.
    (stickies--apply-colors)))

;;;###autoload
(defun stickies-delete ()
  "Delete the current sticky note (with confirmation)."
  (interactive)
  (unless stickies-note-mode
    (user-error "Not in a sticky note buffer"))
  (let* ((buffer (current-buffer))
         (path buffer-file-name)
         (basename (file-name-nondirectory path)))
    (when (yes-or-no-p (format "Delete sticky note %s? " basename))
      (dolist (frame (stickies--frames basename))
        ;; Clear the marker so `stickies--on-frame-deleted' doesn't
        ;; re-save geometry into an entry we're about to drop.
        (set-frame-parameter frame 'stickies-note nil)
        (ignore-errors (delete-frame frame)))
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
      (kill-buffer buffer)
      (delete-file path)
      (stickies--unregister basename))))

(provide 'stickies)
;;; stickies.el ends here
