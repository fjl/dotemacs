;;; htmlize-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "htmlize" "htmlize.el" (0 0 0 0))
;;; Generated autoloads from htmlize.el

(autoload 'htmlize-buffer "htmlize" "\
Convert BUFFER to HTML, preserving colors and decorations.\n\nThe generated HTML is available in a new buffer, which is returned.\nWhen invoked interactively, the new buffer is selected in the current\nwindow.  The title of the generated document will be set to the buffer's\nfile name or, if that's not available, to the buffer's name.\n\nNote that htmlize doesn't fontify your buffers, it only uses the\ndecorations that are already present.  If you don't set up font-lock or\nsomething else to fontify your buffers, the resulting HTML will be\nplain.  Likewise, if you don't like the choice of colors, fix the mode\nthat created them, or simply alter the faces it uses.\n\n(fn &optional BUFFER)" t nil)

(autoload 'htmlize-region "htmlize" "\
Convert the region to HTML, preserving colors and decorations.\nSee `htmlize-buffer' for details.\n\n(fn BEG END)" t nil)

(autoload 'htmlize-file "htmlize" "\
Load FILE, fontify it, convert it to HTML, and save the result.\n\nContents of FILE are inserted into a temporary buffer, whose major mode\nis set with `normal-mode' as appropriate for the file type.  The buffer\nis subsequently fontified with `font-lock' and converted to HTML.  Note\nthat, unlike `htmlize-buffer', this function explicitly turns on\nfont-lock.  If a form of highlighting other than font-lock is desired,\nplease use `htmlize-buffer' directly on buffers so highlighted.\n\nBuffers currently visiting FILE are unaffected by this function.  The\nfunction does not change current buffer or move the point.\n\nIf TARGET is specified and names a directory, the resulting file will be\nsaved there instead of to FILE's directory.  If TARGET is specified and\ndoes not name a directory, it will be used as output file name.\n\n(fn FILE &optional TARGET)" t nil)

(autoload 'htmlize-many-files "htmlize" "\
Convert FILES to HTML and save the corresponding HTML versions.\n\nFILES should be a list of file names to convert.  This function calls\n`htmlize-file' on each file; see that function for details.  When\ninvoked interactively, you are prompted for a list of files to convert,\nterminated with RET.\n\nIf TARGET-DIRECTORY is specified, the HTML files will be saved to that\ndirectory.  Normally, each HTML file is saved to the directory of the\ncorresponding source file.\n\n(fn FILES &optional TARGET-DIRECTORY)" t nil)

(autoload 'htmlize-many-files-dired "htmlize" "\
HTMLize dired-marked files.\n\n(fn ARG &optional TARGET-DIRECTORY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "htmlize" '("htmlize-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; htmlize-autoloads.el ends here
