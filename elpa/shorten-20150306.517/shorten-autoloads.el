;;; shorten-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "shorten" "shorten.el" (21767 16474 0 0))
;;; Generated autoloads from shorten.el

(autoload 'shorten-strings "shorten" "\
Takes a list of strings and returns an alist ((STRING
. SHORTENED-STRING) ...).  Uses `shorten-split-function' to split
the strings, and `shorten-join-function' to join shortened
components back together into SHORTENED-STRING.  See also
`shorten-validate-component-function'.

\(fn STRINGS)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; shorten-autoloads.el ends here
