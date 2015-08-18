(require 'cl-lib)

;; Theme
(setq-default custom-safe-themes t) ;; accept any theme
(load-theme 'slick)

;; Frame parameters for all frames, regardless of window-system.
(setq default-frame-alist
      '((tool-bar-lines 0)
        (left-fringe . nil)
        (right-fringe . 0)
        (vertical-scroll-bars . nil)))

;; Per window-system overrides and additions to default-frame-alist.
(setq window-system-default-frame-alist
      '((ns  . ((menu-bar-lines . 1) (left-fringe . 6) (font . "PragmataPro-14")))
        (mac . ((menu-bar-lines . 1) (left-fringe . 6) (font . "PragmataPro-14")))
        (x   . ((font . "PragmataPro-12") (menu-bar-lines . 0)))
        (t   . ((menu-bar-lines 0)))))

(defun fjl/setup-frame (frame)
  "Reapplies frame parameters from `default-frame-alist' and
`window-system-default-frame-alist'. This is useful while tweaking
and to setup the inital frame."
  (let* ((type (framep-on-display frame))
         (special (assq type window-system-default-frame-alist)))
    (dolist (p default-frame-alist)
      (set-frame-parameter frame (car p) (cdr p)))
    (when special
      (dolist (p (cdr special))
        (set-frame-parameter frame (car p) (cdr p))))))

(defun fjl/setup-all-frames (&optional frame)
  (dolist (frame (frame-list))
    (fjl/setup-frame frame)))

;; Apply the parameters to all frames after creating a new frame.
;; This works around issues I had in the past with the menu bar randomly
;; showing up on a frame.
(add-to-list 'after-make-frame-functions 'fjl/setup-all-frames)

;; Apply the parameters for all initial frames.
(fjl/setup-all-frames)

;; This is a workaround for a bug in the GTK interface
;; where it resets the frame size to something very small
;; on focus out.
(defun fjl/fix-frame-size ()
  (set-frame-size (selected-frame) 9999 9999 t)
  (set-frame-size (selected-frame) 9999 9999 nil))

(defun enable-frame-size-hack ()
  (interactive)
  (add-hook 'focus-in-hook 'fjl/fix-frame-size))

;; OS X stuff
(when (memq window-system '(ns mac))
  (setq-default line-spacing 0.1)
  ;; enable emoji font as fallback
  (set-fontset-font t 'unicode "Symbola" nil 'prepend)
  (setq ns-use-native-fullscreen nil)
  (setq mac-command-modifier 'super)
  (setq ns-command-modifier 'super)
  (setq ns-alternate-modifier 'meta)
  (setq ns-auto-hide-menu-bar nil)
  ;; confirm quit
  (setq confirm-kill-emacs 'y-or-n-p)
)

;; Enable mouse support in terminal
(when (and (not window-system)
           (not (string-equal (getenv "TERM_PROGRAM") "Apple_Terminal")))
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                               (interactive)
                               (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                               (interactive)
                               (scroll-up 1))))

;; UTF-8 terminal
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; The code below shows the mark location as a small rectangle
;; in the fringe. Adapted from http://www.emacswiki.org/emacs/TheFringe.

(defvar fjl/mark-bol nil
  "Marker from `beginning-of-line' for `mark'.")

(put 'fjl/mark-bol 'overlay-arrow-bitmap 'filled-square)
(put 'fjl/mark-bol 'overlay-arrow-string "")
(add-to-list 'overlay-arrow-variable-list 'fjl/mark-bol)

(defun fjl/mark-fringe-hook ()
  (when (and (mark t) (not (minibufferp)))
    (setq fjl/mark-bol
          (save-excursion
            (goto-char (mark t))
            (point-marker)))))

(add-to-list 'activate-mark-hook #'fjl/mark-fringe-hook)
(add-to-list 'buffer-list-update-hook #'fjl/mark-fringe-hook)

;; The Code below renames and hides certain modes
;; in the mode-line to reduce display clutter.

(defvar fjl/mode-line-cleaner-alist
  `((company-mode . "")
    (eldoc-mode . "")
    (paredit-mode . "")
    (abbrev-mode . "")
    (ivy-mode . "")
    (magit-auto-revert-mode . "")

    ;; Major modes
    (lisp-interaction-mode . "λ")
    (hi-lock-mode . "")
    (python-mode . "Py")
    (emacs-lisp-mode . "El")
    (common-lisp-mode . "Cl")
    (js-mode . "Js")
    (nxhtml-mode . "Nx"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun fjl/clean-mode-line ()
  (interactive)
  (cl-loop
   for cleaner in fjl/mode-line-cleaner-alist
   do (let* ((mode (car cleaner))
             (mode-str (cdr cleaner))
             (old-mode-str (cdr (assq mode minor-mode-alist))))
        (when old-mode-str
          (setcar old-mode-str mode-str))
        ;; major mode
        (when (eq mode major-mode)
          (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'fjl/clean-mode-line)

(provide 'init-ui)
