(require 'weechat)

;;;###autoload
(defun weechat-connect-thick ()
  (weechat-connect "thick.fjl.io" 9000 "" "ssh -W localhost:%p %h"))

(defun fjl/weechat-mode-setup ()
  (setq truncate-lines nil)
  (setq word-wrap t))

;;;###autoload
(add-hook 'weechat-mode-hook 'fjl/weechat-mode-setup)
