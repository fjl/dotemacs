;;; solidity-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "solidity-common" "solidity-common.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from solidity-common.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "solidity-common" '("solidity-sol")))

;;;***

;;;### (autoloads nil "solidity-mode" "solidity-mode.el" (0 0 0 0))
;;; Generated autoloads from solidity-mode.el

(add-to-list 'auto-mode-alist '("\\.sol\\'" . solidity-mode))

(autoload 'solidity-mode "solidity-mode" "\
Major mode for editing solidity language buffers.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "solidity-mode" '("solidity-")))

;;;***

;;;### (autoloads nil nil ("solidity-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; solidity-mode-autoloads.el ends here
