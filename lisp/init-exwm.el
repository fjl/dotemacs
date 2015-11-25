(require 'exwm)

(setq use-dialog-box nil)
(server-start)

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
(exwm-input-set-key (kbd "s-r") 'exwm-reset)
;; Switch workspace interactively.
(exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
;; Set shortcuts to switch to a certain workspace.
(exwm-input-set-key (kbd "s-<f1>") (lambda () (interactive) (exwm-workspace-switch 0)))
(exwm-input-set-key (kbd "s-<f2>") (lambda () (interactive) (exwm-workspace-switch 1)))
(exwm-input-set-key (kbd "s-<f3>") (lambda () (interactive) (exwm-workspace-switch 2)))
(exwm-input-set-key (kbd "s-<f4>") (lambda () (interactive) (exwm-workspace-switch 3)))
(exwm-input-set-key (kbd "s-1") 'launcher)

(provide 'init-exwm)
