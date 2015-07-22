;;; editorconfig.el --- EditorConfig Emacs extension

;; Copyright (C) 2011-2013 EditorConfig Team

;; Author: EditorConfig Team <editorconfig@googlegroups.com>
;; Version: 0.3
;; URL: http://github.com/editorconfig/editorconfig-emacs#readme

;; See
;; http://github.com/editorconfig/editorconfig-emacs/graphs/contributors
;; or the CONTRIBUTORS file for the list of contributors.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice,
;;    this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; EditorConfig helps developers define and maintain consistent
;; coding styles between different editors and IDEs.

;; The EditorConfig project consists of a file format for defining
;; coding styles and a collection of text editor plugins that enable
;; editors to read the file format and adhere to defined styles.
;; EditorConfig files are easily readable and they work nicely with
;; version control systems.

;;; Code:

(defvar edconf-exec-path "editorconfig")

(defun edconf-set-indentation (style &optional size tab_width)
  (setq web-mode-indent-style 2)
  "Set indentation type from given style and size"
  (when (equal style "space")
    (setq indent-tabs-mode nil
          size (string-to-number size)
          LaTeX-indent-level size
          LaTeX-item-indent size
          TeX-brace-indent-level size
          c-basic-offset size
          cperl-indent-level size
          haskell-indent-offset size
          shm-indent-spaces size
          js-indent-level size
          js2-basic-offset size
          lisp-indent-offset size
          perl-indent-level size
          py-indent-offset size
          python-indent size
          ruby-indent-level size
          sh-basic-offset size
      		web-mode-markup-indent-offset size
          web-mode-css-indent-offset size
          web-mode-code-indent-offset size
          ;(make-local-variable 'sgml-basic-offset) size
          tab-stop-list (let ((stops (cons size ())))
                          (while (< (car stops) 120)
                            (setq stops (cons
                                         (+ size (car stops))
                                         stops)))
                          (nreverse stops))))
  (when (equal style "tab")
    (setq indent-tabs-mode t))
  (if tab_width
      (setq tab-width (string-to-number tab_width))))

(defun edconf-set-line-ending (end-of-line)
  "Set line ending style to CR, LF, or CRLF"
  (set-buffer-file-coding-system
   (cond
    ((equal end-of-line "lf") 'undecided-unix)
    ((equal end-of-line "cr") 'undecided-mac)
    ((equal end-of-line "crlf") 'undecided-dos)
    (t 'undecided))
   nil t))

(defun edconf-delete-newline-at-eob ()
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (goto-char (1- (point-max)))
        (when (looking-at "\n")
          (delete-char 1))))))

(defun edconf-set-trailing-nl (final-newline)
  "Setup insertion/deletion of final newline."
  (remove-hook 'before-save-hook 'edconf-delete-newline-at-eob t)
  (cond
    ((equal final-newline "true")
      ;; keep prefs around how/when the nl is added, if set - otherwise add on save
      (set (make-local-variable 'require-final-newline)
        (or require-final-newline t))
      (set (make-local-variable 'mode-require-final-newline)
        (or mode-require-final-newline t)))
    ((equal final-newline "false")
      (set (make-local-variable 'require-final-newline) nil)
      (set (make-local-variable 'mode-require-final-newline) nil)
      (add-hook 'before-save-hook 'edconf-delete-newline-at-eob nil t))))

(defun edconf-set-trailing-ws (trim-trailing)
  "Setup trimming of trailing whitespace at end of lines."
  ;; note: delete-trailing-whitespace requires emacs > 21
  (cond
    ((equal trim-trailing "true")
      (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))
    ((equal trim-trailing "false")
      (remove-hook 'before-save-hook 'delete-trailing-whitespace t))))

(defun edconf-properties (file)
  "Get properties for FILE using EditorConfig Core."
  (setq file (expand-file-name file))
  (let (output)
    (with-temp-buffer
      (let ((status (call-process edconf-exec-path nil '(t t) nil file)))
        (if (eql status 0)
            (setq output (buffer-string))
          (error "EditorConfig error (%S): %s" status (buffer-string)))))
    (unless (zerop (length output))
      (let ((props (make-hash-table)))
        (dolist (line (split-string output "\n" t) props)
          (let* ((key-val (split-string line "="))
                 (val (apply #'concat (cdr key-val))))
            (puthash (intern (car key-val)) val props)))))))

;;;###autoload
(defun edconf-setup-buffer (&optional buffer interactive-p)
  "Apply settings from matching .editorconfig files in BUFFER.
If BUFFER is nil, setup the current buffer."
  (interactive "i\np")
  (let* ((buffer (or buffer (current-buffer)))
         (file   (buffer-file-name buffer)))
    (cond
      ((not file)
        (when interactive-p
          (message "Buffer does not visit a file.")))
      ((not (executable-find edconf-exec-path))
        (when interactive-p
          (warn "No such executable: %s" edconf-exec-path)))
      (t
        (let ((props (edconf-properties file)))
          (if (null props)
            (when interactive-p
              (message "No .editorconfig settings for this file."))
            (with-current-buffer buffer
              (edconf-set-indentation
                (gethash 'indent_style props)
                (gethash 'indent_size props)
                (gethash 'tab_width props))
              (edconf-set-line-ending
                (gethash 'end_of_line props))
              (edconf-set-trailing-nl
                (gethash 'insert_final_newline props))
              (edconf-set-trailing-ws
                (gethash 'trim_trailing_whitespace props)))
            (when interactive-p
              (message "Applied .editorconfig settings."))))))))

;;;###autoload
(defun edconf-setup-all-buffers ()
  "Apply settings from matching .editorconfig files in all buffers."
  (interactive)
  (mapc #'edconf-setup-buffer (buffer-list)))

;;;###autoload
(add-hook 'after-change-major-mode-hook 'edconf-setup-buffer)

(provide 'editorconfig)

;;; editorconfig.el ends here
