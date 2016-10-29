;;; solidity-mode.el --- Major mode for ethereum's solidity language

;; Copyright (C) 2015  Lefteris Karapetsas

;; Author: Lefteris Karapetsas  <lefteris@refu.co>
;; Keywords: languages
;; Package-Version: 20161017.1506
;; Version: 0.1.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Solidity is a high-level language whose syntax is similar to that
;; of Javascript and it is designed to compile to code for the
;; Ethereum Virtual Machine.

;; This package provides a major mode for writing Solidity code.

;;; Code:

(require 'cc-mode)


;;; --- Customizable variables ---
(defgroup solidity nil
  "Major mode for ethereum's solidity language"
  :group 'languages ;; Emacs -> Programming -> Languages
  :prefix "solidity-"
  :link '(url-link :tag "Github" "https://github.com/ethereum/emacs-solidity"))

(defcustom solidity-mode-hook nil
  "Callback hook to execute whenever a solidity file is loaded."
  :group 'solidity)

(defcustom solidity-solc-path "/usr/bin/solc"
  "Path to the solc binary."
  :group 'solidity
  :type 'string
  :package-version '(solidity . "0.1.1"))

(defvar solidity-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for solidity major mode.")

(defvar solidity-checker t "The solidity flycheck syntax checker.")
(defvar solidity-mode t "The solidity major mode.")
(defvar flycheck-solidity-executable t "The solc executable used by flycheck.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sol\\'" . solidity-mode))

(defun solidity-filter (condp lst)
  "A simple version of a list filter.  Depending on CONDP filter LST."
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defconst solidity-keywords
  '("after"
    "as"
    "assembly"
    "break"
    "constant"
    "anonymous"
    "continue"
    "contract"
    "default"
    "delete"
    "do"
    "else"
    "event"
    "external"
    "for"
    "function"
    "if"
    "import"
    "in"
    "is"
    "indexed"
    "library"
    "mapping"
    "modifier"
    "new"
    "pragma"
    "private"
    "public"
    "internal"
    "return"
    "returns"
    "struct"
    "switch"
    "this"
    "using"
    "var"
    "while"
    "enum"
    "throw"
    )
  "Keywords of the solidity language.")

(defconst solidity-tofontify-keywords
  (solidity-filter
   (lambda (x) (not (member x '("contract"
                                "library"))))
   solidity-keywords)
  "Keywords that will be fontified as they are and don't have special rules."
  )

(defconst solidity-constants
  '("true" "false"
    "wei"
    "szabo"
    "finney"
    "ether"
    "seconds"
    "minutes"
    "hours"
    "days"
    "weeks"
    "years"
    )
  "Constants in the solidity language.")

(defconst solidity-builtin-types
  '("address"
    "bool"
    "bytes"
    "bytes0"
    "bytes1"
    "bytes2"
    "bytes3"
    "bytes4"
    "bytes5"
    "bytes6"
    "bytes7"
    "bytes8"
    "bytes9"
    "bytes10"
    "bytes11"
    "bytes12"
    "bytes13"
    "bytes14"
    "bytes15"
    "bytes16"
    "bytes17"
    "bytes18"
    "bytes19"
    "bytes20"
    "bytes21"
    "bytes22"
    "bytes23"
    "bytes24"
    "bytes25"
    "bytes26"
    "bytes27"
    "bytes28"
    "bytes29"
    "bytes30"
    "bytes31"
    "bytes32"
    "int"
    "int8"
    "int16"
    "int24"
    "int32"
    "int40"
    "int48"
    "int56"
    "int64"
    "int72"
    "int80"
    "int88"
    "int96"
    "int104"
    "int112"
    "int120"
    "int128"
    "int136"
    "int144"
    "int152"
    "int160"
    "int168"
    "int176"
    "int184"
    "int192"
    "int200"
    "int208"
    "int216"
    "int224"
    "int232"
    "int240"
    "int248"
    "int256"

    "mapping"
    "real"
    "string"
    "text"

    "uint"
    "uint8"
    "uint16"
    "uint24"
    "uint32"
    "uint40"
    "uint48"
    "uint56"
    "uint64"
    "uint72"
    "uint80"
    "uint88"
    "uint96"
    "uint104"
    "uint112"
    "uint120"
    "uint128"
    "uint136"
    "uint144"
    "uint152"
    "uint160"
    "uint168"
    "uint176"
    "uint184"
    "uint192"
    "uint200"
    "uint208"
    "uint216"
    "uint224"
    "uint232"
    "uint240"
    "uint248"
    "uint256"

    "ureal")
  "Built in data types of the solidity language.")

(defconst solidity-builtin-constructs
  '("msg"
    "block"
    "tx")
  "Built in constructs of the solidity language.")

(defvar solidity-identifier-regexp
  "\\([a-zA-z0-9]\\|_\\)+")

(defvar solidity-variable-attributes
  "\\(&\\|*\\|~\\)"
  "Variable attributes like references '&' e.t.c.")

;; Set font lock options.
;; For information on the various faces check here:
;; http://www.gnu.org/software/emacs/manual/html_node/ccmode/Faces.html
;; For examples on how to make advanced fontification based on the
;; language rules check C mode here:
;; http://cc-mode.sourceforge.net/src/cc-fonts.el
;;
;; Guide for Searh based fontification:
;; http://ergoemacs.org/emacs_manual/elisp/Search_002dbased-Fontification.html
;; General colouring guide:
;; http://ergoemacs.org/emacs/elisp_syntax_coloring.html
(defconst solidity-font-lock-keywords
  (list
   `(,(regexp-opt solidity-tofontify-keywords 'words) . font-lock-keyword-face)
   `(,(regexp-opt solidity-builtin-types 'words) . font-lock-type-face)
   `(,(regexp-opt solidity-builtin-constructs 'words) . font-lock-builtin-face)
   '(solidity-match-functions (1 font-lock-type-face)
                              (2 font-lock-function-name-face))
   '(solidity-match-mappings (1 font-lock-type-face)
                             (2 font-lock-function-name-face))
   '(solidity-match-pragma-stmt (1 font-lock-preprocessor-face)
                                 (2 font-lock-string-face))
   '(solidity-match-library-decl (1 font-lock-keyword-face)
                                 (2 font-lock-variable-name-face))
   '(solidity-match-contract-decl (1 font-lock-keyword-face)
                                  (2 font-lock-variable-name-face))
   '(solidity-match-modifier-decl (1 font-lock-keyword-face)
                                (2 font-lock-variable-name-face))
   '(solidity-match-struct-decl (1 font-lock-keyword-face)
                                (2 font-lock-variable-name-face))
   '(solidity-match-event-decl (1 font-lock-keyword-face)
                                  (2 font-lock-variable-name-face))
   '(solidity-match-variable-decls (1 font-lock-keyword-face)
                                   (2 font-lock-variable-name-face))
   `(,(regexp-opt solidity-constants 'words) . font-lock-constant-face))
  "The font lock options for solidity.")

(defun solidity-match-regexp (re limit)
  "Generic regular expression matching wrapper for RE with a given LIMIT."
  (re-search-forward re
                     limit ; search bound
                     t     ; no error, return nil
                     nil   ; do not repeat
                     ))

(defun solidity-match-contract-decl (limit)
  "Search the buffer forward until LIMIT matching contract declarations.

First match should be a keyword and second an identifier."
  (solidity-match-regexp
   (concat
    " *\\(contract\\) *\\(" solidity-identifier-regexp "\\)")
   limit))

(defun solidity-match-library-decl (limit)
  "Search the buffer forward until LIMIT matching library declarations.

First match should be a keyword and second an identifier."
  (solidity-match-regexp
   (concat
    " *\\(library\\) *\\(" solidity-identifier-regexp "\\)")
   limit))

(defun solidity-match-pragma-stmt (limit)
  "Search the buffer forward until LIMIT matching pragma statements.

First match should be a keyword and second an identifier."
  (solidity-match-regexp
   (concat
    " *\\(pragma\\) *\\(.*\\);")
   limit))

(defun solidity-match-struct-decl (limit)
  "Search the buffer forward until LIMIT matching struct declarations.

First match should be a keyword and second an identifier."
  (solidity-match-regexp
   (concat
    " *\\(struct\\) *\\(" solidity-identifier-regexp "\\)")
   limit))

(defun solidity-match-functions (limit)
  "Search the buffer forward until LIMIT matching function names.

Highlight the 1st result."
  (solidity-match-regexp
   (concat
    " *\\(function\\) *\\(" solidity-identifier-regexp "\\)")
   limit))

(defun solidity-match-event-decl (limit)
  "Search the buffer forward until LIMIT matching function names.

Highlight the 1st result."
  (solidity-match-regexp
   (concat
    " *\\(event\\) *\\(" solidity-identifier-regexp "\\)")
   limit))

(defun solidity-match-modifier-decl (limit)
  "Search the buffer forward until LIMIT matching function names.

Highlight the 1st result."
  (solidity-match-regexp
   (concat
    " *\\(modifier\\) *\\(" solidity-identifier-regexp "\\)")
   limit))

(defun solidity-match-mappings (limit)
  "Search the buffer forward until LIMIT matching solidity mappings.

Highlight the 1st result."
  (solidity-match-regexp
   (concat
    " *\\(mapping\\) *(.*) *\\(" solidity-identifier-regexp "\\)")
   limit))

(defun solidity-match-variable-decls (limit)
  "Search the buffer forward until LIMIT matching variable declarations.

Highlight the 1st result."
  (solidity-match-regexp
   (concat
    " *\\(" (regexp-opt solidity-builtin-types) "\\) *\\(" solidity-identifier-regexp "\\)")
   limit))

;; solidity syntax table
(defvar solidity-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; '_' underscore is a valid part of a word
    (modify-syntax-entry ?_ "w" st)
    ;; c++ style comments in the syntax table
    ;; more info on the syntax flags here:
    ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Flags.html
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for the solidity language.")

;;;###autoload
(define-derived-mode solidity-mode c-mode "solidity"
  "Major mode for editing solidity language buffers."
  (set-syntax-table solidity-mode-syntax-table)
  ;; specify syntax highlighting
  (setq font-lock-defaults '(solidity-font-lock-keywords))
  ;; register indentation functions, basically the c-mode ones
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)

  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (make-local-variable 'adaptive-fill-mode)
  (make-local-variable 'adaptive-fill-regexp)
  (make-local-variable 'fill-paragraph-handle-comment)

  ;; now set their values
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'indent-line-function) 'c-indent-line)
  (set (make-local-variable 'indent-region-function) 'c-indent-region)
  (set (make-local-variable 'normal-auto-fill-function) 'c-do-auto-fill)
  (set (make-local-variable 'comment-multi-line) t)
  (set (make-local-variable 'comment-line-break-function)
       'c-indent-new-comment-line)
  (run-hooks 'solidity-mode-hook))

;;; --- interface with flycheck if existing ---
(when (eval-when-compile (require 'flycheck nil 'noerror))
  ;; add dummy source-inplace definition to avoid errors
  (defvar source-inplace t)
  ;; add a solidity mode callback to set the executable of solc for flycheck
  ;; define solidity's flycheck syntax checker
  (flycheck-define-checker solidity-checker
    "A Solidity syntax checker using the solc compiler"
    :command ("/usr/bin/solc" source-inplace)
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ":" " Error: " (message))
     (warning line-start "Error: " (message)))
    :modes solidity-mode
    :predicate (lambda () (eq major-mode 'solidity-mode)))
  (add-to-list 'flycheck-checkers 'solidity-checker)
  (add-hook 'solidity-mode-hook
            (lambda () (setq flycheck-solidity-checker-executable solidity-solc-path))))

(provide 'solidity-mode)
;;; solidity-mode.el ends here
