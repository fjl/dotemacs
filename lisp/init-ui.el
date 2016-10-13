(require 'cl-lib)
(eval-when-compile (require 'cl)) ;; for lexical-let*

;; Theme
(setq-default custom-safe-themes t) ;; accept any theme
(load-theme 'slick)

;; Make ANSI color look nice with the theme.

(defun fjl/setup-ansi-color-theme ()
  (aset ansi-color-map 34 '(foreground-color . "deep sky blue")))

(add-hook 'eshell-mode-hook 'fjl/setup-ansi-color-theme)
(add-hook 'shell-mode-hook 'fjl/setup-ansi-color-theme)

;; Font
(defmacro fjl/define-font-class (name &rest alternates)
  (let ((alts (cl-gensym)) (sel (cl-gensym)))
    `(lexical-let* ((,alts ',alternates)
                    (,sel  (or (when (functionp 'x-family-fonts)
                                 (cl-find-if 'x-family-fonts ,alts))
                               (car (last ,alts)))))
       (defun ,name (&optional size)
         (or (and size (format "%s-%d" ,sel size))
             ,sel)))))

(fjl/define-font-class fpfont "PragmataPro" "Dejavu Sans Mono" "Consolas" "Monospace")
(fjl/define-font-class vpfont "Avenir" "Noto Sans" "Dejavu Sans" "Sans Serif")

;; Frame parameters for all frames, regardless of window-system.
(setq default-frame-alist
      '((tool-bar-lines 0)
        (left-fringe . nil)
        (right-fringe . 0)
        (menu-bar-lines . 0)
        (vertical-scroll-bars . nil)))

;; Per window-system overrides and additions to default-frame-alist.
(setq window-system-default-frame-alist
      `((ns  . ((menu-bar-lines . 1) (left-fringe . 6) (font . ,(fpfont 14))))
        (mac . ((menu-bar-lines . 1) (left-fringe . 6) (font . ,(fpfont 14))))
        (w32 . ((font . ,(fpfont 12))))
        (x   . ((font . ,(fpfont 12)) (left-fringe . 6)))))

(set-face-attribute 'fixed-pitch nil :family (fpfont))
(set-face-attribute 'variable-pitch nil :family (vpfont) :height 1.0)

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

(defun fjl/setup-mac-gui ()
  "Applies macOS settings."
  (setq-default line-spacing 0.1)
  (setq ns-use-native-fullscreen nil)
  ;; enable emoji font as fallback
  (set-fontset-font t 'unicode "Symbola" nil 'prepend)
  ;; keyboard settings
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)
  (setq mac-mouse-wheel-smooth-scroll nil)
  (setq ns-command-modifier 'super)
  (setq ns-alternate-modifier 'meta)
  (setq ns-auto-hide-menu-bar nil)
  (when (eq window-system 'mac)
    ;; enable ligatures
    (when (functionp 'mac-auto-operator-composition-mode)
      (setq mac-auto-operator-composition-characters "!\"#$%&'()+,-/:;<=>?@[]^_`{|}~")
      (mac-auto-operator-composition-mode))
    ;; save/restore frame configuration on mac port.
    (desktop-save-mode 1)
    (setq desktop-save t)))

(when (memq window-system '(ns mac))
  (fjl/setup-mac-gui))

(when (memq window-system '(gtk))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; Display margin content on the inside of the fringe.
;; It looks nicer.
(setq-default fringes-outside-margins t)

;; Make linum look nicer in combination with the above
;; setting. This mostly adds some space after the number.
(progn
  (defvar fjl/linum-fmt-width 0)
  (defun fjl/linum-numbering-hook ()
    (setq fjl/linum-fmt-width (length (number-to-string (count-lines (point-min) (point-max))))))
  (defun fjl/linum-format (n)
    (propertize (format (concat "%" (number-to-string fjl/linum-fmt-width) "d ") n)
                'face 'linum))
  (add-hook 'linum-before-numbering-hook 'fjl/linum-numbering-hook)
  (setq-default linum-format #'fjl/linum-format))

;; Scrolling in compilation-mode

(defun fjl/compilation-mode-hook ()
  ;; These auto scroll settings make it so point isn't
  ;; centered in the window when output reaches the bottom.
  (setq-local scroll-conservatively 200)
  (setq-local scroll-step 1))

(add-hook 'compilation-mode-hook 'fjl/compilation-mode-hook)

;; Mode Line Setup.

(defun fjl/mode-line-align-right (face format)
  (let* ((fmt     (format-mode-line format face))
         (reserve (string-width fmt)))
    (list (propertize " " 'display `((space :align-to (- (+ right right-margin right-fringe) ,reserve))))
          fmt)))

(setq-default mode-line-format
              '(("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
                 mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
                 (vc-mode vc-mode)
                 "  " mode-line-modes
                 (:eval (fjl/mode-line-align-right nil mode-line-misc-info)))))

;; Terminal

(defun xterm-title-update (&optional title)
  (when (eq t (framep-on-display))
    (send-string-to-terminal (concat "\033]0;" (or title (buffer-name)) "\007"))))

(defun xterm-title-clear ()
  (xterm-title-update ""))

(unless window-system
  (xterm-mouse-mode 1)
  (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 1)))
  (add-hook 'window-configuration-change-hook 'xterm-title-update)
  (add-hook 'kill-emacs-hook 'xterm-title-clear)
  (xterm-title-update))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Rename and hide certain modes in the mode-line to reduce display clutter.
(defvar fjl/mode-line-cleaner-alist
  `(;; Minor modes that should be hidden.
    (company-mode . "")
    (eldoc-mode . "")
    (paredit-mode . "")
    (abbrev-mode . "")
    (ivy-mode . "")
    (magit-auto-revert-mode . "")
    (buffer-face-mode . "")

    ;; Major modes.
    (markdown-mode . "Md")
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
        (when (eq mode major-mode)
          (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'fjl/clean-mode-line)

(provide 'init-ui)
