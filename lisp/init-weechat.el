(require 'weechat)

;;;###autoload
(defun weechat-connect-crick ()
  (interactive)
  (with-temp-buffer
    (setq weechat-password-callback (lambda (host port) (message "password callback") ""))
    (weechat-connect "crick.fjl.io" 9000 "" "ssh -W localhost:%p %h" t)))

;;;###autoload
(defun fjl/weechat-mode-setup ()
  ;; Hack to enable reconnect.
  (setq-local weechat-password-callback (lambda (host port) ""))
  (setq truncate-lines nil)
  (setq word-wrap t)
  (visual-line-mode 1)
  (require 'weechat-tracking))

;;;###autoload
(add-hook 'weechat-mode-hook 'fjl/weechat-mode-setup)
