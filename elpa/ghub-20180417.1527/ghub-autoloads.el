;;; ghub-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ghub" "ghub.el" (0 0 0 0))
;;; Generated autoloads from ghub.el

(autoload 'ghub-create-token "ghub" "\
Create, store and return a new token.\n\nHOST is the Github instance, usually \"api.github.com\".\nUSERNAME is the name of a user on that instance.\nPACKAGE is the package that will use the token.\nSCOPES are the scopes the token is given access to.\n\n(fn HOST USERNAME PACKAGE SCOPES)" t nil)

(autoload 'ghub-token-scopes "ghub" "\
Return and echo the scopes of the specified token.\nThis is intended for debugging purposes only.  The user\nhas to provide several values including their password.\n\n(fn HOST USERNAME PACKAGE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ghub" '("auth-source-netrc-parse-next-interesting@save-match-data" "ghub-")))

;;;***

;;;### (autoloads nil nil ("ghub-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ghub-autoloads.el ends here
