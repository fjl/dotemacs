(require 'w3m)
(require 'w3m-search)
(require 'init-commands)

(setq w3m-use-favicon t)
(setq w3m-use-tab nil)
(setq w3m-enable-google-feeling-lucky nil)
(add-to-list 'w3m-search-engine-alist '("duckduckgo" "https://duckduckgo.com/?q=%s"))
(setq w3m-search-default-engine "duckduckgo")

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
(defun fjl/w3m-mode-hook ()
  (setq word-wrap t)
  (setq truncate-lines nil)
  (when (> (window-text-width) 90)
    (set-visual-wrap-column 90)))

;;;###autoload
(add-hook 'w3m-mode-hook 'fjl/w3m-mode-hook)
