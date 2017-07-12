;; -*- lexical-binding: t -*-

(require 'cc-mode)

;;;###autoload
(defun fjl/c++-mode-hook ()
  (prettify-symbols-mode)
  (c-add-style "ethereum"
               '("bsd"
                 (indent-tabs-mode . t)
                 (tab-width . 4)
                 (c-basic-offset . 4)
                 (c-hanging-braces-alist
                  (substatement-open before)
                  (brace-list-open))
                 (c-hanging-colons-alist
                  (member-init-intro before)
                  (inher-intro)
                  (case-label after)
                  (label after)
                  (access-label after))
                 (c-cleanup-list scope-operator empty-defun-braces defun-close-semi)
                 (c-offsets-alist
                  (namespace-open . 0)
                  (namespace-close . 0)
                  (innamespace . 0)
                  (arglist-close . 0)
                  (arglist-cont . 0)
                  (inlambda . 0)
                  (lambda-intro-cont . 0)
                  (substatement-open . 0)
                  (inline-open . 0)))))

;;;###autoload
(add-hook 'c++-mode-hook 'fjl/c++-mode-hook)

(provide 'init-cc-mode)
