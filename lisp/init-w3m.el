(require 'w3m)
 ;; this seems to be required, compilation doesn't exit otherwise.
(require 'w3m-cookie)

(setq w3m-use-favicon t)
(setq w3m-use-tab nil)

(defun w3m-window-p (w)
  (equal (buffer-name (window-buffer w)) "*w3m*"))

;;;###autoload
(defun w3m-browse-url-other-window (url &rest args)
  (if (one-window-p)
      (apply #'w3m-browse-url url args)
    (let ((w3m-win (get-window-with-predicate #'w3m-window-p
                                              'exclude-minibuffer
                                              'visible
                                              (get-lru-window))))
      (with-selected-window w3m-win
        (apply #'w3m-browse-url url args)))))

;;;###autoload
(setq browse-url-browser-function 'w3m-browse-url-other-window)
