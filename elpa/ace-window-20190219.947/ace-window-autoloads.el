;;; ace-window-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ace-window" "ace-window.el" (0 0 0 0))
;;; Generated autoloads from ace-window.el

(autoload 'ace-select-window "ace-window" "\
Ace select window.\n\n(fn)" t nil)

(autoload 'ace-delete-window "ace-window" "\
Ace delete window.\n\n(fn)" t nil)

(autoload 'ace-swap-window "ace-window" "\
Ace swap window.\n\n(fn)" t nil)

(autoload 'ace-delete-other-windows "ace-window" "\
Ace delete other windows.\n\n(fn)" t nil)

(autoload 'ace-window "ace-window" "\
Select a window.\nPerform an action based on ARG described below.\n\nBy default, behaves like extended `other-window'.\n\nPrefixed with one \\[universal-argument], does a swap between the\nselected window and the current window, so that the selected\nbuffer moves to current window (and current buffer moves to\nselected window).\n\nPrefixed with two \\[universal-argument]'s, deletes the selected\nwindow.\n\n(fn ARG)" t nil)

(defvar ace-window-display-mode nil "\
Non-nil if Ace-Window-Display mode is enabled.\nSee the `ace-window-display-mode' command\nfor a description of this minor mode.\nSetting this variable directly does not take effect;\neither customize it (see the info node `Easy Customization')\nor call the function `ace-window-display-mode'.")

(custom-autoload 'ace-window-display-mode "ace-window" nil)

(autoload 'ace-window-display-mode "ace-window" "\
Minor mode for showing the ace window key in the mode line.\n\n(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ace-window" '("ace-window-mode" "aw-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ace-window-autoloads.el ends here
