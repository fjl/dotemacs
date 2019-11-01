(require 'init-bootstrap)

(defvar bitlbee-socket "/tmp/bitlbee.socket")

;;;###autoload
(defun erc-connect-bitlbee ()
  (interactive)
  (let ((erc-server-connect-function 'fjl/erc-connect-unix-socket))
    (erc :server bitlbee-socket :port 0 :nick (user-login-name) :password "")))

(defun fjl/erc-connect-unix-socket (name buffer host service)
  (make-network-process :name name :buffer buffer
                        :service host :family 'local :nowait t))

(provide 'init-erc)
