(require 'init-bootstrap)
(require 's)

;; Quick hack to switch GTK text scale factor.
(defun dconf-write (path value)
  (shell-command (format "dconf write %s '%S'" path value)))
(defun dconf-read-number (path)
  (string-to-number (shell-command-to-string (format "dconf read %s" path))))

;;;###autoload
(defun toggle-retina ()
  "Toggles the GTK font scale factor between 1.0 and 1.4."
  (interactive)
  (let ((cur (dconf-read-number "/org/gnome/desktop/interface/text-scaling-factor")))
    (cond ((= cur 1)
           (dconf-write "/org/gnome/desktop/interface/text-scaling-factor" 1.4))
          (t
           (dconf-write "/org/gnome/desktop/interface/text-scaling-factor" 1.0)))))

;;;###autoload
(defun xcape-reset ()
  "Restarts the xcape daemon."
  (interactive)
  (shell-command (format "killall xcape; %s/scripts/run-xcape.sh" user-emacs-directory) nil))

(defvar backlight-brightness-steps 20)
(defvar backlight-brightness-command "/usr/lib/gnome-settings-daemon/gsd-backlight-helper")

(defun backlight-brightness-at-step (step max)
  (round (* (/ (float max) backlight-brightness-steps backlight-brightness-steps) step step)))

(defun backlight-brightness-closest-step (cur max)
  (let ((step 0))
    (while (> cur (backlight-brightness-at-step step max))
      (setq step (1+ step)))
    step))

(defun backlight-brightness-bar (step)
  (concat (s-repeat step "█") (s-repeat (- backlight-brightness-steps step) "░")))

(defun backlight-brightness-read (flag)
  (string-to-number
   (ignore-errors
     (shell-command-to-string
      (concat backlight-brightness-command " " flag)))))

(defun backlight-brightness-write (value)
  ;; Use pkexec to run the helper as root.
  ;; gnome-settings-daemon installs the appropriate policy so this should work.
  (shell-command
   (format "pkexec %s --set-brightness %d" backlight-brightness-command value)))

;;;###autoload
(defun adjust-backlight-brightness (inc)
  "Adjusts backlight brightness through gnome-settings-daemon."
  (interactive "N")
  (let ((max (backlight-brightness-read "--get-max-brightness"))
        (cur (backlight-brightness-read "--get-brightness")))
    (if (or (not max) (not cur))
        (message "can't get brightness")
      (let* ((step    (backlight-brightness-closest-step cur max))
             (newstep (max 0 (min backlight-brightness-steps (+ step inc))))
             (new     (backlight-brightness-at-step newstep max)))
        (backlight-brightness-write new)
        (message "Display Backlight: %s" (backlight-brightness-bar newstep))))))

;;;###autoload
(defun backlight-brightness-up ()
  (interactive)
  (adjust-backlight-brightness 1))

;;;###autoload
(defun backlight-brightness-down ()
  (interactive)
  (adjust-backlight-brightness -1))

(provide 'init-x11-commands)
