;;; org-re-reveal-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from org-re-reveal.el

(autoload 'org-re-reveal-publish-to-reveal "org-re-reveal" "\
Publish an Org file to HTML.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.  Optional BACKEND may specify a derived export
backend.
Return output file name.

(fn PLIST FILENAME PUB-DIR &optional BACKEND)")
(autoload 'org-re-reveal-publish-to-reveal-client "org-re-reveal" "\
Publish an Org file to HTML as multiplex client.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.  Optional BACKEND may specify a derived export
backend.
If `org-re-reveal-client-multiplex-filter' is non-nil, use it as regular
expression to only publish FILENAME if it matches this regular expression.
Return output file name.

(fn PLIST FILENAME PUB-DIR &optional BACKEND)")
(autoload 'org-re-reveal-version "org-re-reveal" "\
Display version string for org-re-reveal from Lisp file." t)
(register-definition-prefixes "org-re-reveal" '("org-re-reveal-"))

;;; End of scraped data

(provide 'org-re-reveal-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; org-re-reveal-autoloads.el ends here
