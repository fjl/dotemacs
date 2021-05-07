(require 'company)
(require 'eldoc)

;;;###autoload
(defun fjl/elisp-mode-hook ()
  (define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)
  (eldoc-mode)
  (company-mode))

;;;###autoload
(add-hook 'emacs-lisp-mode-hook 'fjl/elisp-mode-hook)
