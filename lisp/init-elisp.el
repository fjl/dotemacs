(require 'company)
(require 'eldoc)

;;;###autoload
(defun fjl/elisp-mode-hook ()
  (eldoc-mode)
  (company-mode))

;;;###autoload
(add-hook 'emacs-lisp-mode-hook 'fjl/elisp-mode-hook)

 
