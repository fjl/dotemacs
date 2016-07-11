;; -*- lexical-binding: t; -*-
(require 'exwm)
(require 'exwm-randr)
(require 'exwm-systemtray)

(setq use-dialog-box nil)
(server-start)

;; Disable auto update of keysyms to remove awkward
;; typing pause after pressing space.
(setq xcb:keysyms:auto-update nil)

;; Enable XRANDR support. I don't really want to use the hook though, it's just needed so
;; exwm resizes its frames when the display config changes.
(exwm-randr-enable)

;; Enable the system tray.
(exwm-systemtray-enable)

;; Rename buffers to window title.
(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

;; We always need a way to go back to line-mode from char-mode.
(exwm-input-set-key (kbd "C-c r") 'exwm-reset)
;; Switch workspace interactively.
(exwm-input-set-key (kbd "C-c v") 'exwm-workspace-switch)
(exwm-input-set-key (kbd "C-c M-v") 'exwm-workspace-move-window)
;; Set shortcuts to switch to a certain workspace.
(exwm-input-set-key (kbd "s-<f1>") (lambda () (interactive) (exwm-workspace-switch 0)))
(exwm-input-set-key (kbd "s-<f2>") (lambda () (interactive) (exwm-workspace-switch 1)))
(exwm-input-set-key (kbd "s-<f3>") (lambda () (interactive) (exwm-workspace-switch 2)))
(exwm-input-set-key (kbd "s-<f4>") (lambda () (interactive) (exwm-workspace-switch 3)))
(exwm-input-set-key (kbd "s-S-<f1>") (lambda () (interactive) (exwm-workspace-move-window 0)))
(exwm-input-set-key (kbd "s-S-<f2>") (lambda () (interactive) (exwm-workspace-move-window 1)))
(exwm-input-set-key (kbd "s-S-<f3>") (lambda () (interactive) (exwm-workspace-move-window 2)))
(exwm-input-set-key (kbd "s-S-<f4>") (lambda () (interactive) (exwm-workspace-move-window 3)))
(exwm-input-set-key (kbd "C-c <f1>") (lambda () (interactive) (exwm-workspace-switch 0)))
(exwm-input-set-key (kbd "C-c <f2>") (lambda () (interactive) (exwm-workspace-switch 1)))
(exwm-input-set-key (kbd "C-c <f3>") (lambda () (interactive) (exwm-workspace-switch 2)))
(exwm-input-set-key (kbd "C-c <f4>") (lambda () (interactive) (exwm-workspace-switch 3)))
(exwm-input-set-key (kbd "C-c 1") 'launcher)
(exwm-input-set-key (kbd "s-1") 'launcher)

(exwm-input-set-simulation-keys
 '(([?\C-b] . left)
   ([?\C-f] . right)
   ([?\C-p] . up)
   ([?\C-n] . down)
   ([?\C-a] . home)
   ([?\C-e] . end)
   ([?\M-v] . prior)
   ([?\C-v] . next)))

(defun sh! (command &rest args)
  (lambda ()
    (interactive)
    (apply 'start-process command nil command args)))

(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") (sh! "brightness" "down"))
(exwm-input-set-key (kbd "<XF86MonBrightnessUp>")   (sh! "brightness" "up"))
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")  (sh! "amixer" "set" "Master" "8%+"))
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>")  (sh! "amixer" "set" "Master" "8%-"))
(exwm-input-set-key (kbd "s-l") (sh! "loginctl" "lock-session"))

;; Flush key bindings. This should happen by default, but doesn't.
(exwm-input--update-global-prefix-keys)

(provide 'init-exwm)
