(require 'weechat)

;;;###autoload
(defun weechat-connect-thick ()
  (interactive)
  (with-temp-buffer
    (setq-local weechat-password-callback (lambda () ""))
    (weechat-connect "thick.fjl.io" 9000 "" "ssh -W localhost:%p %h")))

(defun fjl/weechat-mode-setup ()
  ;; Hack to enable reconnect.
  (setq-local weechat-password-callback (lambda () ""))
  (setq truncate-lines nil)
  (setq word-wrap t)
  (visual-line-mode 1)
  (require 'weechat-tracking))

;;;###autoload
(add-hook 'weechat-mode-hook 'fjl/weechat-mode-setup)
