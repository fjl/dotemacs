;;; go-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "go-mode" "go-mode.el" (0 0 0 0))
;;; Generated autoloads from go-mode.el

(autoload 'go-mode "go-mode" "\
Major mode for editing Go source text.\n\nThis mode provides (not just) basic editing capabilities for\nworking with Go code. It offers almost complete syntax\nhighlighting, indentation that is almost identical to gofmt and\nproper parsing of the buffer content to allow features such as\nnavigation by function, manipulation of comments or detection of\nstrings.\n\nIn addition to these core features, it offers various features to\nhelp with writing Go code. You can directly run buffer content\nthrough gofmt, read godoc documentation from within Emacs, modify\nand clean up the list of package imports or interact with the\nPlayground (uploading and downloading pastes).\n\nThe following extra functions are defined:\n\n- `gofmt'\n- `godoc' and `godoc-at-point'\n- `go-import-add'\n- `go-remove-unused-imports'\n- `go-goto-arguments'\n- `go-goto-docstring'\n- `go-goto-function'\n- `go-goto-function-name'\n- `go-goto-imports'\n- `go-goto-return-values'\n- `go-goto-method-receiver'\n- `go-play-buffer' and `go-play-region'\n- `go-download-play'\n- `godef-describe' and `godef-jump'\n- `go-coverage'\n- `go-set-project'\n- `go-reset-gopath'\n\nIf you want to automatically run `gofmt' before saving a file,\nadd the following hook to your emacs configuration:\n\n(add-hook 'before-save-hook #'gofmt-before-save)\n\nIf you want to use `godef-jump' instead of etags (or similar),\nconsider binding godef-jump to `M-.', which is the default key\nfor `find-tag':\n\n(add-hook 'go-mode-hook (lambda ()\n                          (local-set-key (kbd \"M-.\") #'godef-jump)))\n\nPlease note that godef is an external dependency. You can install\nit with\n\ngo get github.com/rogpeppe/godef\n\n\nIf you're looking for even more integration with Go, namely\non-the-fly syntax checking, auto-completion and snippets, it is\nrecommended that you look at flycheck\n(see URL `https://github.com/flycheck/flycheck') or flymake in combination\nwith goflymake (see URL `https://github.com/dougm/goflymake'), gocode\n(see URL `https://github.com/nsf/gocode'), go-eldoc\n(see URL `github.com/syohex/emacs-go-eldoc') and yasnippet-go\n(see URL `https://github.com/dominikh/yasnippet-go')\n\n(fn)" t nil)

(add-to-list 'auto-mode-alist (cons "\\.go\\'" 'go-mode))

(autoload 'gofmt-before-save "go-mode" "\
Add this to .emacs to run gofmt on the current buffer when saving:\n(add-hook 'before-save-hook 'gofmt-before-save).\n\nNote that this will cause ‘go-mode’ to get loaded the first time\nyou save any file, kind of defeating the point of autoloading.\n\n(fn)" t nil)

(autoload 'godoc "go-mode" "\
Show Go documentation for QUERY, much like \\<go-mode-map>\\[man].\n\n(fn QUERY)" t nil)

(autoload 'go-download-play "go-mode" "\
Download a paste from the playground and insert it in a Go buffer.\nTries to look for a URL at point.\n\n(fn URL)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "go-mode" '("gofmt" "god" "go-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; go-mode-autoloads.el ends here
