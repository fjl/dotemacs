;; -*- lexical-binding: t -*-

(require 'cl-lib)
(eval-when-compile
  (require 'cl) ;; for lexical-let*
  (require 'desktop))

;; Theme
(setq-default custom-safe-themes t) ;; accept any theme
(load-theme 'slick)

;; Make ANSI color look nice with the theme.

(defun fjl/setup-ansi-color-theme ()
  (aset ansi-color-map 34 '(foreground-color . "deep sky blue")))

(add-hook 'eshell-mode-hook 'fjl/setup-ansi-color-theme)
(add-hook 'shell-mode-hook 'fjl/setup-ansi-color-theme)

;; Fonts

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

(set-face-attribute 'fixed-pitch nil :family (fpfont))
(set-face-attribute 'variable-pitch nil :family (vpfont) :height 1.0)

(defvar fjl/setting-up-first-frame t
  "This variable is set to nil after setting up the first frame.")

(defun fjl/prettify-symbols-compose-p (start end _match)
  "Return true iff the symbol MATCH should be composed. This
function is just like `prettify-symbols-default-compose-p', but
also enables prettification in comments."
  (let* ((syntaxes-beg (if (memq (char-syntax (char-after start)) '(?w ?_))
                           '(?w ?_) '(?. ?\\)))
         (syntaxes-end (if (memq (char-syntax (char-before end)) '(?w ?_))
                           '(?w ?_) '(?. ?\\))))
    (not (or (memq (char-syntax (or (char-before start) ?\s)) syntaxes-beg)
             (memq (char-syntax (or (char-after end) ?\s)) syntaxes-end)))))

(defun fjl/setup-pragmata-ligatures ()
  (when (string-prefix-p "Pragmata" (fpfont))
    (setq-default prettify-symbols-alist
                  '(("<-"     . ?🡐)
                    ("!="     . "	")
                    ("*="     . "	")
                    ("+="     . "	")
                    ("=="     . "	")
                    (":="     . "	")
                    ("|="     . "	")
                    ("&="     . "	")
                    ("^="     . "	")
                    ("//"     . "	")
                    ("/*"     . "	")
                    ("*/"     . "	")
                    ("<="     . "	")
                    (">="     . "	")
                    ("&&"     . "	")
                    ("||"     . "	")
                    ("::"     . "	")
                    ("///"    . "	")
                    ("TODO:"  . "	 ")
                    ("BUG:"   . "	")
                    ("NOTE:"  . "	")
                    ("FIXME:" . "	")
                    ("TODO"   . "	 ")
                    ("BUG"    . "	")
                    ("NOTE"   . "	")
                    ("FIXME"  . "	")))
    (setq-default prettify-symbols-compose-predicate
                  'fjl/prettify-symbols-compose-p)))

;; Mac/NS Display

;; (quieten the byte compiler)
(defvar mac-command-modifier)
(defvar mac-option-modifier)
(defvar mac-mouse-wheel-mode)
(defvar mac-mouse-wheel-smooth-scroll)
(defvar mac-drawing-use-gcd)
(defvar mac-auto-operator-composition-characters)
(defvar ns-use-native-fullscreen)
(defvar ns-alternate-modifier)
(defvar ns-command-modifier)
(defvar ns-auto-hide-menu-bar)

(defun fjl/setup-mac-gui (&optional frame)
  "Applies macOS gui settings."
  (setq-default line-spacing 0.1)
  (setq ns-use-native-fullscreen nil)
  (setq ns-pop-up-frames nil)
  (setq frame-resize-pixelwise t)
  ;; enable emoji font as fallback
  (set-fontset-font t 'unicode "Symbola" frame 'prepend)
  ;; keyboard settings
  (setq ns-command-modifier 'super)
  (setq ns-alternate-modifier 'meta)
  (fjl/setup-pragmata-ligatures)
  (when (eq window-system 'mac)
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier 'meta)
    (setq mac-mouse-wheel-mode t)
    (setq mac-mouse-wheel-smooth-scroll nil)
    (setq mac-drawing-use-gcd nil)))

(defun fjl/mac-path-helper-path ()
  (with-temp-buffer
    (let ((process-environment
           (cons "PATH"
                 (cl-remove-if (lambda (s) (string-prefix-p "PATH=" s)) process-environment))))
      (process-file "/usr/libexec/path_helper" nil (current-buffer) nil))
    (zap-to-char -1 ?\") ;; trim "\nexport PATH; at end
    (goto-char (point-min))
    (zap-to-char 1 ?\") ;; trim PATH=" at beginning
    (buffer-string)))

(defun fjl/mac-app-resources-bin ()
  (save-match-data
    (let ((cmd (first command-line-args)))
      (when (string-match "\\.app/Contents/\\(MacOS/Emacs\\)$" cmd)
        (replace-match "Resources/bin" t t cmd 1)))))

(defun fjl/setup-mac-path ()
  "Sets executable path variables according to /etc/paths.
Applications started via launchd get the system default PATH
which isn't very useful."
  (let ((path     (fjl/mac-path-helper-path))
        (tool-bin (fjl/mac-app-resources-bin))
        (home-bin (expand-file-name "~/bin")))
    (when (file-exists-p home-bin)
      (setq path (concat path ":" home-bin)))
    (when tool-bin
      (setq path (concat tool-bin ":" path)))
    (setenv "PATH" path)
    (setq exec-path (nconc (split-string path ":") (last exec-path)))))

(defun fjl/setup-mac (&optional frame)
  (fjl/setup-mac-gui frame)
  (when fjl/setting-up-first-frame
    (fjl/setup-mac-path)
    ;; Save/restore frame configuration.
    (desktop-save-mode 1)
    (setq desktop-save 'if-exists)))

;; GTK Display

(defun fjl/setup-gtk (&optional frame)
  "Applies GTK gui settings."
  (set-fontset-font t 'unicode "Symbola" frame 'prepend)
  (fjl/setup-pragmata-ligatures))

;; TTY Display

(defun xterm-title-update (&optional title)
  (when (eq t (framep-on-display))
    (send-string-to-terminal (concat "\033]0;" (or title (buffer-name)) "\007"))))

(defun xterm-title-clear ()
  (xterm-title-update ""))

(defun fjl/setup-tty ()
  (xterm-mouse-mode 1)
  (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 1)))
  (add-hook 'window-configuration-change-hook 'xterm-title-update)
  (add-hook 'kill-emacs-hook 'xterm-title-clear)
  (xterm-title-update))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Frame parameters for all frames, regardless of window-system.
(setq default-frame-alist
      '((tool-bar-lines 0)
        (left-fringe . nil)
        (right-fringe . 0)
        (menu-bar-lines . 0)
        (vertical-scroll-bars . nil)))

;; Per window-system overrides and additions to default-frame-alist.
(setq window-system-default-frame-alist
      `((ns  . ((menu-bar-lines . 1) (left-fringe . 6) (font . ,(fpfont 14)) (alpha 93) (ns-appearance . dark) (ns-transparent-titlebar . t)))
        (mac . ((menu-bar-lines . 1) (left-fringe . 6) (font . ,(fpfont 14)) (alpha 93)))
        (w32 . ((font . ,(fpfont 12))))
        (x   . ((font . ,(fpfont 12)) (left-fringe . 6)))))

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
        (set-frame-parameter frame (car p) (cdr p))))
    (cond ((or (eq type 'ns) (eq type 'mac))  (fjl/setup-mac frame))
          ((eq type 'x)                       (fjl/setup-gtk frame))
          ((eq type t)                        (fjl/setup-tty)))
    (setq fjl/setting-up-first-frame nil)))

(defun fjl/setup-all-frames (&rest _)
  (dolist (frame (frame-list))
    (fjl/setup-frame frame)))

;; Apply the parameters to all frames after creating a new frame.
;; This works around issues I had in the past with the menu bar randomly
;; showing up on a frame.
(add-to-list 'after-make-frame-functions 'fjl/setup-all-frames)

;; Apply the parameters for all initial frames.
(fjl/setup-all-frames)

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

;; For the compilation buffer, these auto scroll settings make it so point isn't centered
;; in the window when output reaches the bottom.
(defun fjl/compilation-mode-hook ()
  (setq-local scroll-conservatively 200)
  (setq-local scroll-step 1))

(add-hook 'compilation-mode-hook 'fjl/compilation-mode-hook)

;; When quitting server edits, kill the window with them
;; if it was opened for the edit.
(add-hook 'server-done-hook 'quit-window)

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
