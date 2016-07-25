;; -*- lexical-binding: t; -*-
(require 'exwm)
(require 'exwm-randr)
(require 'exwm-systemtray)
(require 'exim)

;; Enable exwm.
(setq use-dialog-box nil)
(server-start)
(exwm-enable)

;; Set configuration variables
(setq exwm-workspace-show-all-buffers nil)
(setq exwm-layout-show-all-buffers nil)

;; Disable auto update of keysyms to remove awkward
;; typing pause after pressing space.
(setq xcb:keysyms:auto-update nil)

;; Enable XRANDR support. I don't really want to use the hook though, it's just needed so
;; exwm resizes its frames when the display config changes.
(exwm-randr-enable)

;; Enable the system tray.
(exwm-systemtray-enable)

;; Enable EXIM when exwm is started and pass
;; through toggle-input-method.
(add-hook 'exwm-init-hook 'exim-start)
(push ?\C-\\ exwm-input-prefix-keys)

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

;; Set up emacs emulation bindings for line mode.
(exwm-input-set-simulation-keys
 `(([?\C-b] . left)
   ([?\C-f] . right)
   ([?\C-p] . up)
   ([?\C-n] . down)
   ([?\M-<] . home)
   ([?\M->] . end)
   ([?\M-w] . ?\C-c)
   ([?\C-y] . ?\C-v)))

(defun fjl/shell-lambda (command)
  "Returns an interactive function which runs command as a local shell command."
  (lambda ()
    (interactive)
    (let ((default-directory "~/"))
      (shell-command command))))

;; Set up global bindings.
(progn
  (exwm-input-set-key (kbd "C-c r") 'exwm-reset)
  (exwm-input-set-key (kbd "<C-escape>") 'exwm-reset)
  ;; Switch workspace interactively.
  (exwm-input-set-key (kbd "C-c v") 'exwm-workspace-switch)
  (exwm-input-set-key (kbd "C-c M-v") 'exwm-workspace-move-window)
  ;; Set shortcuts to switch to a certain workspace.
  (exwm-input-set-key (kbd "s-<f1>") (lambda () (interactive) (exwm-workspace-switch-create 0)))
  (exwm-input-set-key (kbd "s-<f2>") (lambda () (interactive) (exwm-workspace-switch-create 1)))
  (exwm-input-set-key (kbd "s-<f3>") (lambda () (interactive) (exwm-workspace-switch-create 2)))
  (exwm-input-set-key (kbd "s-<f4>") (lambda () (interactive) (exwm-workspace-switch-create 3)))
  (exwm-input-set-key (kbd "s-S-<f1>") (lambda () (interactive) (exwm-workspace-move-window 0)))
  (exwm-input-set-key (kbd "s-S-<f2>") (lambda () (interactive) (exwm-workspace-move-window 1)))
  (exwm-input-set-key (kbd "s-S-<f3>") (lambda () (interactive) (exwm-workspace-move-window 2)))
  (exwm-input-set-key (kbd "s-S-<f4>") (lambda () (interactive) (exwm-workspace-move-window 3)))
  (exwm-input-set-key (kbd "C-c <f1>") (lambda () (interactive) (exwm-workspace-switch-create 0)))
  (exwm-input-set-key (kbd "C-c <f2>") (lambda () (interactive) (exwm-workspace-switch-create 1)))
  (exwm-input-set-key (kbd "C-c <f3>") (lambda () (interactive) (exwm-workspace-switch-create 2)))
  (exwm-input-set-key (kbd "C-c <f4>") (lambda () (interactive) (exwm-workspace-switch-create 3)))
  ;; Launching programs.
  (exwm-input-set-key (kbd "s-1") 'launcher)
  ;; Brightness/audio.
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") 'backlight-brightness-down)
  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>")   'backlight-brightness-up)
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")  (fjl/shell-lambda (concat user-emacs-directory "scripts/pa-vol.sh plus")))
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>")  (fjl/shell-lambda (concat user-emacs-directory "scripts/pa-vol.sh minus")))
  (exwm-input-set-key (kbd "<XF86AudioMute>")         (fjl/shell-lambda (concat user-emacs-directory "scripts/pa-vol.sh mute")))
  ;; Screen Lock
  (exwm-input-set-key (kbd "s-l") (fjl/shell-lambda "loginctl lock-session"))
  ;; Flush key bindings. This should happen by default, but doesn't.
  (exwm-input--update-global-prefix-keys))

(provide 'init-exwm)
