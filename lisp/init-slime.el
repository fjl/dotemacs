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
   slime-fancy
   slime-asdf
   slime-quicklisp
   slime-editing-commands
   slime-package-fu
   slime-autodoc
   slime-fuzzy
   slime-fancy-inspector
   slime-company))

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

;; Set up tramp stuff.
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

(setq slime-from-lisp-filename-function 'slime-tramp-remote-filename)
(setq slime-to-lisp-filename-function 'slime-tramp-local-filename)

(defun fjl/directory-asdf-systems (directory)
  (let (found)
    (locate-dominating-file
     directory
     (lambda (dir)
       (dolist (f (directory-files dir nil "\.asd$"))
         (cl-pushnew (file-name-sans-extension f) found :test 'equal))
       found))
    found))

(defun fjl/slime-test-system (system)
  (interactive
   (list
    (let* ((directory (or default-directory (file-name-directory (buffer-file-name))))
           (systems (fjl/directory-asdf-systems directory)))
      (cond ((null (cdr systems))
             ;; Exactly one system, don't prompt.
             (car systems))
            (current-prefix-arg
             (completing-read "Test ASDF system: " systems))
            ((null systems)
             (error "Can't find ASDF system in %s." directory))
            (t
             ;; No prefix arg, just use the first available system.
             (car systems)))))
   slime-mode)
  (slime-oos system 'test-op))

;; Keys
(define-key slime-mode-map (kbd "C-x 9") 'fjl/slime-test-system)
(define-key slime-mode-map (kbd "C-c M-q") 'slime-reindent-defun)
(define-key slime-repl-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)

;;;###autoload
(defun fjl/slime-mode-hook ()
  (global-set-key (kbd "C-c C-s") 'slime-selector)
  (global-set-key (kbd "C-c s") 'slime-selector)
  (setq-local browse-url-browser-function 'eww-browse-url)
  (setq tab-always-indent 'complete)
  (company-mode 1))

;;;###autoload
(defun fjl/common-lisp-mode-hook ()
  (setq common-lisp-style-default "fjl-indentation"))

;;;###autoload
(progn
  (add-hook 'slime-mode-hook 'fjl/slime-mode-hook)
  (add-hook 'common-lisp-mode-hook 'fjl/common-lisp-mode-hook))

(provide 'init-slime)
