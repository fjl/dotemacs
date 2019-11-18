;; -*- lexical-binding: t -*-

(require 'slime)
(require 'tramp)
(eval-when-compile
  (require 'slime-indentation)
  (require 'slime-asdf))

(setq
 slime-default-lisp 'sbcl
 slime-lisp-implementations
 '((sbcl  ("/usr/bin/env" "LC_ALL=UTF-8" "/usr/local/bin/sbcl")
          :coding-system utf-8-unix)
   (ccl   ("~/bin/ccl64"))
   (clisp ("clisp"))
   (ecl   ("ecl"))))

(slime-setup
 '(slime-indentation
   slime-fancy slime-asdf
   slime-editing-commands
   slime-package-fu
   slime-autodoc
   slime-fuzzy
   slime-fancy-inspector))

(define-common-lisp-style "fjl-indentation"
  "My personal indentation style. cond gets indented similar to case."
  (:inherit "classic")
  (:variables
   (lisp-loop-indent-subclauses t)
   (lisp-lambda-list-keyword-alignment t)
   (lisp-lambda-list-keyword-parameter-alignment t)
   (indent-tabs-mode nil)
   (fill-column 80))
  (:indentation
   (cond (&rest (&whole 2 &rest 3)))))

(defun slime-tramp-local-filename (f)
  (if (file-remote-p f)
      (tramp-file-name-localname (tramp-dissect-file-name f))
    f))

(defun slime-tramp-remote-filename (f)
  (if (file-remote-p default-directory)
      (let ((tf (tramp-dissect-file-name default-directory)))
        (tramp-make-tramp-file-name
         (tramp-file-name-method tf)
         (tramp-file-name-user tf)
         (tramp-file-name-domain tf)
         (tramp-file-name-host tf)
         (tramp-file-name-port tf)
         f))
    f))

(defun fjl/directory-asdf-system (directory)
  (let ((files (directory-files directory nil "\.asd$")))
    (when files
      (file-name-sans-extension (car files)))))

(defun slime-test-current-system ()
  (interactive)
  (let* ((directory (or default-directory (file-name-directory (buffer-file-name))))
         (system (fjl/directory-asdf-system directory)))
    (if system
        (slime-oos system 'test-op)
      (message "Can't determine ASDF system."))))

;;;###autoload
(defun fjl/slime-connected-hook ()
  (setq slime-from-lisp-filename-function 'slime-tramp-remote-filename)
  (setq slime-to-lisp-filename-function 'slime-tramp-local-filename))

;;;###autoload
(defun fjl/slime-mode-hook ()
  (global-set-key (kbd "C-c C-s") 'slime-selector)
  (global-set-key (kbd "C-c s") 'slime-selector)
  (setq-local browse-url-browser-function 'eww-browse-url)
  (define-key slime-mode-map (kbd "C-x 9") 'slime-test-current-system)
  (define-key slime-mode-map (kbd "C-c M-q") 'slime-reindent-defun)
  (setq tab-always-indent 'complete))

;;;###autoload
(defun fjl/common-lisp-mode-hook ()
  (setq common-lisp-style-default "fjl-indentation"))

;;;###autoload
(add-hook 'slime-connected-hook 'fjl/slime-connected-hook)
;;;###autoload
(add-hook 'slime-mode-hook 'fjl/slime-mode-hook)
;;;###autoload
(add-hook 'common-lisp-mode-hook 'fjl/common-lisp-mode-hook)

(provide 'init-slime)
