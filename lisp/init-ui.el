;; -*- lexical-binding: t -*-

(require 'init-bootstrap)
(require 'cl-lib)

(eval-when-compile
  (require 'ivy)
  (require 'ansi-color))

;; Theme
(setq-default custom-safe-themes t) ;; accept any theme
(load-theme 'slick)

;; Fonts

(defmacro fjl/define-font-class (name &rest alternates)
  (let ((selection (intern (concat "fjl/selected-" (symbol-name name)))))
    `(progn
       (defvar ,selection
         (or (when (functionp 'x-family-fonts)
               (cl-find-if 'x-family-fonts ',alternates))
             (car (last ',alternates))))
       (defun ,name (&optional size)
         (if (not size) ,selection (format "%s-%d" ,selection size))))))

(fjl/define-font-class fpfont "PragmataPro" "Dejavu Sans Mono" "Consolas" "Monospace")
(fjl/define-font-class vpfont "Avenir" "Noto Sans" "Dejavu Sans" "Sans Serif")
(fjl/define-font-class emojifont "Apple Color Emoji" "Symbola")

(set-face-attribute 'fixed-pitch nil :family (fpfont))
(set-face-attribute 'variable-pitch nil :family (vpfont) :height 1.0)

;; Make mode line fixed-pitch (default is variable-pitch on emacs >= 29.1).
(when (get 'mode-line-active 'face)
  (set-face-attribute 'mode-line-active nil :inherit 'mode-line))
(when (get 'mode-line-inactive 'face)
  (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line))

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
  (when (string-prefix-p "PragmataPro" (fpfont))
    (setq-default prettify-symbols-alist
                  '(("<-"     . " ←")
                    ("->"     . " →")))
    (setq-default prettify-symbols-compose-predicate
                  'fjl/prettify-symbols-compose-p)))

;; Mac/NS Display

;; (quieten the byte compiler)
(defvar mac-command-modifier)
(defvar mac-right-command-modifier)
(defvar mac-option-modifier)
(defvar mac-mouse-wheel-mode)
(defvar mac-mouse-wheel-smooth-scroll)
(defvar mac-pass-control-to-system)
(defvar mac-ignore-accessibility)
(defvar ns-pop-up-frames)
(defvar ns-auto-hide-menu-bar)
(defvar ns-use-native-fullscreen)
(defvar ns-alternate-modifier)
(defvar ns-command-modifier)
(defvar ns-right-command-modifier)

(defun fjl/setup-mac-gui (&optional frame)
  "Applies macOS gui settings."
  (setq ns-use-native-fullscreen nil)
  (setq ns-pop-up-frames nil)
  (setq ns-auto-hide-menu-bar nil)
  (setq frame-resize-pixelwise t)
  ;; enable emoji font as fallback and other font settings
  (set-fontset-font t 'unicode (emojifont 10) frame 'prepend)
  (setq-default line-spacing 0.1)
  (fjl/setup-pragmata-ligatures)
  ;; keyboard settings
  (if (eq window-system 'mac)
    (setq mac-command-modifier 'super
          mac-right-command-modifier 'meta
          mac-option-modifier 'meta
          mac-mouse-wheel-mode t
          mac-mouse-wheel-smooth-scroll nil
          mac-pass-control-to-system nil
          mac-ignore-accessibility t)
    (setq ns-command-modifier 'super
          ns-right-command-modifier 'meta
          ns-alternate-modifier 'meta)))

(defun fjl/mac-path-helper-path ()
  (with-temp-buffer
    (let ((process-environment
           (cons "PATH"
                 (cl-remove-if (lambda (s) (string-prefix-p "PATH=" s)) process-environment))))
      (process-file "/usr/libexec/path_helper" nil (current-buffer) nil))
    (let (start end)
      (goto-char (point-min))
      (setq start (search-forward "\""))
      (setq end (1- (search-forward "\"")))
      (buffer-substring start end))))

(defun fjl/mac-app-resources-bin ()
  (save-match-data
    (let ((cmd (car command-line-args)))
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
    (fjl/setup-mac-path)))

;; GTK Display

(defun fjl/setup-gtk (&optional frame)
  "Applies GTK gui settings."
  (set-fontset-font t 'unicode "Symbola" frame 'prepend)
  (fjl/setup-pragmata-ligatures))

;; TTY Display

(defun xterm-title-update (title)
  (when (eq t (framep-on-display))
    (send-string-to-terminal (concat "\033]0;" title "\007"))))

(defun xterm-title-hook-fn (&optional frame-or-window)
  "This function is called through various hooks during redisplay
to set the terminal title."
  (let ((buffer (window-buffer (frame-selected-window frame-or-window))))
    (unless (minibufferp buffer)
      (xterm-title-update (buffer-name buffer)))))

(defun xterm-title-clear ()
  (xterm-title-update ""))

(defun fjl/mouse-scroll-down-1 ()
  (interactive)
  (scroll-down 1))

(defun fjl/mouse-scroll-up-1 ()
  (interactive)
  (scroll-up 1))

(defun fjl/setup-tty ()
  (xterm-mouse-mode 1)
  (global-set-key [mouse-4] 'fjl/mouse-scroll-down-1)
  (global-set-key [mouse-5] 'fjl/mouse-scroll-up-1)
  (if (not (boundp 'window-buffer-change-functions))
      ;; Emacs < 27 only has window-configuration-change-hook, which doesn't
      ;; trigger for buffer changes.
      (add-hook 'window-configuration-change-hook 'xterm-title-hook-fn)
    ;; Emacs >= 27 has fine-grained window change functions.
    (add-hook 'window-selection-change-functions 'xterm-title-hook-fn)
    (add-hook 'window-buffer-change-functions 'xterm-title-hook-fn))
  (add-hook 'kill-emacs-hook 'xterm-title-clear)
  (xterm-title-hook-fn nil))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(defun fjl/setup-frame (frame)
  "Reapplies frame parameters from `default-frame-alist' and
`window-system-default-frame-alist'. This is useful while tweaking
and to setup the inital frame."
  (let* ((type (framep-on-display frame))
         (special (assq type window-system-default-frame-alist)))
    (modify-frame-parameters frame (append default-frame-alist
                                           (when special (cdr special))))
    (cond ((or (eq type 'ns) (eq type 'mac))      (fjl/setup-mac frame))
          ((eq type 'x)                           (fjl/setup-gtk frame))
          ((and (eq type t) (not noninteractive)) (fjl/setup-tty)))
    (setq fjl/setting-up-first-frame nil)))

(defun fjl/setup-all-frames (&rest _)
  (dolist (frame (frame-list))
    (fjl/setup-frame frame)))

;; Apply the parameters to all frames after creating a new frame.
;; This works around issues I had in the past with the menu bar randomly
;; showing up on a frame.
(add-to-list 'after-make-frame-functions 'fjl/setup-all-frames)

(progn
  ;; Frame parameters for all frames, regardless of window-system.
  (setq default-frame-alist
        '((tool-bar-lines 0)
          (left-fringe . nil)
          (right-fringe . 0)
          (vertical-scroll-bars . nil)
          (child-frame-border-width . 20)))
  ;; Per window-system overrides and additions to default-frame-alist.
  (setq window-system-default-frame-alist
        `((ns  . ((menu-bar-lines . 1)
                  (left-fringe . 6)
                  (font . ,(fpfont 14))
                  (alpha . 98)
                  (ns-transparent-titlebar . nil)
                  (ns-appearance . dark)))
          (mac . ((menu-bar-lines . 1)
                  (left-fringe . 6)
                  (internal-border-width . 2)
                  (font . ,(fpfont 14))
                  (alpha . 98)))
          (w32 . ((font . ,(fpfont 12))
                  (menu-bar-lines . 0)))
          (x   . ((font . ,(fpfont 12))
                  (left-fringe . 6)
                  (menu-bar-lines . 0)))
          (t   . ((menu-bar-lines . 0)))))
  ;; Apply the parameters for all initial frames.
  (fjl/setup-all-frames))

;; Display margin content on the inside of the fringe.
;; It looks nicer.
(setq-default fringes-outside-margins t)

;; For the compilation buffer, these auto scroll settings make it so point isn't centered
;; in the window when output reaches the bottom.
(defun fjl/compilation-mode-hook ()
  (setq-local scroll-conservatively 200)
  (setq-local scroll-step 1)
  (when (functionp 'ansi-color-compilation-filter)
    (unless (member major-mode '(ag-mode grep-mode))
      (setq-local compilation-filter-hook
                  (cons 'ansi-color-compilation-filter compilation-filter-hook)))))

(add-hook 'compilation-mode-hook 'fjl/compilation-mode-hook)

;; Enable winner mode.
(winner-mode 1)

;; Colorize buffer names in ivy buffer switcher.
(after-package ivy
  (add-to-list 'ivy-switch-buffer-faces-alist '(markdown-mode . ivy-org)))

;; Set up the default mode line.
(setq-default mode-line-format
              '(("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
                 mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
                 (vc-mode vc-mode)
                 "  " mode-line-modes)
                (:eval
                 (when (fjl/bottom-right-window-p)
                   (fjl/mode-line-align-right nil mode-line-misc-info)))))

(defun fjl/bottom-right-window-p (&optional window)
  "Returns whether the selected window is the one in the
bottom right corner of its frame."
  (when (null window)
    (setq window (selected-window)))
  (let ((frame (window-frame window))
        (edges (window-edges window)))
    (and (>= (nth 2 edges) (frame-width frame))
         (>= (nth 3 edges) (- (frame-height frame)
                              (window-height (minibuffer-window frame)))))))

(defun fjl/mode-line-align-right (face format)
  (let* ((fmt     (format-mode-line format face))
         (reserve (string-width fmt)))
    (list (propertize " " 'display `((space :align-to (- (+ right right-margin right-fringe) ,reserve))))
          fmt)))

;; Shorten projectile mode-line display.
;; (after-package projectile
;;   (defun fjl/projectile-mode-line ()
;;     (if (file-remote-p default-directory)
;;         " P"
;;       (format " P[%s]" (projectile-project-name))))
;;   (setq projectile-mode-line-function 'fjl/projectile-mode-line))

;; Rename and hide certain modes in the mode-line to reduce display clutter.
(defvar fjl/mode-line-cleaners
  (let ((table (make-hash-table :test 'eq)))
    (prog1 table
      (mapc (lambda (kv) (setf (gethash (car kv) table) (cdr kv)))
            ;; Minor modes that should be hidden.
            '((abbrev-mode "")
              (dtrt-indent-mode " DI")
              (auto-revert-mode "")
              (buffer-face-mode "")
              (company-mode "")
              (dired-async-mode "")
              (eldoc-mode "")
              (hi-lock-mode "")
              (ivy-mode "")
              (magit-auto-revert-mode "")
              (paredit-mode "")
              (with-editor-mode "")
              ;; Major modes.
              (common-lisp-mode "Cl")
              (emacs-lisp-mode "El")
              (js-mode "Js")
              (lisp-interaction-mode "λ")
              (markdown-mode "Md")
              (nxhtml-mode "Nx")
              (python-mode "Py")
              (scheme-mode "Scm"))))))

(defun fjl/clean-mode-line ()
  (interactive)
  ;; Replace minor mode strings.
  (dolist (mm minor-mode-alist)
    (let* ((mode (car mm))
           (mode-desc (cadr mm))
           (replacement (gethash mode fjl/mode-line-cleaners)))
      (cond ((and (eq mode 'projectile-mode) (not (and (consp mode-desc) (eq (car mode-desc) :eval))))
             ;; Hack: remove "Projectile" in utility buffers.
             (setcdr mm `((:eval (unless (string= (format-mode-line ,mode-desc) " Projectile")
                                   ,mode-desc)))))
            (replacement
             ;; Use the replacement string.
             (setcdr mm replacement)))))
  ;; Replace major mode string.
  (let ((r (gethash major-mode fjl/mode-line-cleaners)))
    (when r (setq mode-name (car r)))))

(add-hook 'after-change-major-mode-hook 'fjl/clean-mode-line)

;; Set up display-buffer behavior.
(setq display-buffer-base-action
      '((display-buffer-reuse-window display-buffer-in-previous-window)
        (reusable-frames . visible)))

(provide 'init-ui)
