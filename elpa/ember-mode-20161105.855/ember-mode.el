;;; ember-mode.el --- Ember navigation mode for emacs

;;;;;;;;;;;;;;;;
;;;; MIT License

;; Copyright (C) 2014 Aad Versteden
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;;;;;;;;;;;;;;
;;;; Accounting

;; Version: 0.2.3
;; Package-Version: 20161105.855
;; Author: Aad Versteden <madnificent@gmail.com>
;; Keywords: ember ember.js emberjs
;; License: MIT
;; Package-Requires: ((cl-lib "0.5"))

;;; Commentary:
;; This is a proof of concept for ember-mode.  ember-mode helps you
;; navigate through the files in your emberjs project.  A bunch of
;; bindings have been created to quickly jump to the relevant sources
;; given that you're visiting a recognised file (*) in the ember project.
;;
;; In the current state, you can quickly jump to the:
;; - model
;; - controller
;; - route
;; - router
;; - view
;; - component
;; - template
;;
;; ember-mode is currently geared towards ember-cli, however the
;; folder structure is similar in similar build systems for ember so
;; it will probably work there as well.
;;
;;
;; (*) There is a base implementation for the file recognition, but it
;;     needs improvement so you can always jump back from a found file.
;;     Some (somewhat) less common files are not recognised yet.

;;; Code:
(require 'cl-lib)

(defgroup ember nil
  "Ember-mode customizations."
  :prefix "ember-"
  :group 'tools)

;;;;;;;;;;;;
;;;; plurals
;;
;; This should really be replaced by a Snowball or Porter2 stemmer.
;; It seems to be good enough for a proof of concept of ember-mode.

(defcustom ember-pluralization-irregular-nouns
  '(("child" . "children") ("woman" . "women") ("man" . "men") ("mouse" . "mice") ("goose" . "geese"))
  "Contain irregular pluralizations which ember-mode considers."
  :type '(alist :key-type string :value-type string)
  :group 'ember)

(defun ember--pluralize-noun (noun)
  "Pluralizes NOUN."
  (save-match-data
    (cond ((cl-find noun ember-pluralization-irregular-nouns :key #'car :test #'string=)
           (cdr (cl-find noun ember-pluralization-irregular-nouns :key #'car :test #'string=)))
          ((string-match-p "[yo]$" noun)
           (message "Don't know how to translate %s" noun)
           noun)
          ((or (string-match "ch$" noun)
               (string-match "[xs]$" noun))
           (concat noun "es"))
          ((string-match "^\\(.*\\)fe?$" noun)
           (concat (match-string 1 noun) "ves"))
          (t (concat noun "s")))))

(defun ember--singularize-noun (noun)
  "Singularizes NOUN."
  (save-match-data
    (cond ((cl-find noun ember-pluralization-irregular-nouns :key #'cdr :test #'string=)
           (car (cl-find noun ember-pluralization-irregular-nouns :key #'cdr :test #'string=)))
          ((string-match "^\\(.*ch\\)es$" noun)
           (match-string 1 noun))
          ((string-match "^\\(.*[xs]\\)es$" noun)
           (match-string 1 noun))
          ((string-match "^\\(.*\\)ves$" noun)
           (concat (match-string 1 noun) "f")) ;; this is just a wild guess, it might as well be fe
          ((string-match "^\\(.*\\)s$" noun)
           (match-string 1 noun))
          (t noun))))


;;;;;;;;;;;;;;;;;;;;
;;; General Settings
(defcustom ember-script-file-types
  '("coffee" "js")
  "Filetypes used for script files.  These are the javascript and the coffeescript file.

The first item in this list is used as the 'default', indicating
the preference to look up this type of file."
  :type '(repeat string)
  :group 'ember)

(defcustom ember-template-file-types
  '("hbs" "html" "handlebars")
  "Filetypes used for snippet files.  These are the handlebars and html source files.

The first item in this list is used as the 'default', used when creating files."
  :type '(repeat string)
  :group 'ember)

(defcustom ember-keymap-prefix (kbd "C-c .")
  "Ember keymap prefix."
  :group 'ember
  :type 'key-sequence
  :set
  (lambda (option value)
    (when (boundp 'ember-mode-keymap)
      (define-key ember-mode-keymap ember-keymap-prefix nil)
      (define-key ember-mode-keymap value 'ember-command-prefix))
    (set-default 'ember-keymap-prefix value)))

(defcustom ember-completion-system 'ido
  "Which completion system ember-mode should use."
  :group 'ember
  :type '(radio
          (const :tag "Ido" ido)
          (const :tag "Helm" helm)
          (const :tag "Default" default)))

(defcustom ember-command "ember"
  "Ember command"
  :group 'ember
  :type 'string)


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; POD structure support

(defvar ember-use-pods 'unset
  "Default to not using the POD structure for now.")

(defun ember--get-matcher-map-name ()
  "Returns the name of the map for the current matcher.

Returns either 'pod and 'no-pod."
  (cond
    ((eq ember-use-pods t) 'pod)
    ((eq ember-use-pods nil) 'no-pod)
    (t (if (ember--dot-ember-cli-has-pods-p)
           'pod 'no-pod))))

(defun ember--dot-ember-cli-has-pods-p ()
  "Returns non-nil iff the .ember-cli file in the root sets
usePods to true.  Very basic detection is performed to see if the
line with usePods is commented out."
  (let ((ember-cli-filename (concat (ember--current-project-root)
                                    "/.ember-cli")))
    (with-temp-buffer
      (insert-file-contents ember-cli-filename)
      (re-search-forward "^[^/]*\"usePods\".*:.*true" nil t))))

;;;;;;;;;;;;;;
;;; Navigation

(defvar *ember--matcher-templates* (make-hash-table)
  "Contains a hash with lists of file templates.  The key of the
list is the name of the map (no-pod or pod).

A file template is a list containing:
- the base type    (eg: component)
- the target kind  (eg: source)
- the file path  (eg: (list \"app/components/\" :class \".\" :extension))

It is assumed that the base-type and target-kind regexes don't
contain parens, `ember--relative-file-components' makes use of this
assumption.

From the string base, a type can be built.")

(cl-defun ember--define-matcher (map-name base-type-regex target-kind base-location &optional (base-type base-type-regex))
  "Adds a matcher to the end of the list of *EMBER--MATCHER-TEMPLATES* for map-name"
  (setf (gethash map-name *ember--matcher-templates*)
        (append (gethash map-name *ember--matcher-templates*)
                (list (list base-type-regex target-kind base-location base-type)))))

(defmacro ember--define-matchers (matcher-map-name &rest matchers)
  `(progn ,@(cl-loop for matcher in matchers
               collect
                 `(ember--define-matcher ',matcher-map-name
                                         ,(car matcher) ,(cadr matcher)
                                         (list ,@(cl-caddr matcher))
                                         ,@(cl-cdddr matcher)))))

(ember--define-matchers no-pod
   ;; BEGIN contains the definition for each matcher
   ;; the first two columns are a regexp, the rest is executed as code
   ;; base-type  | target-kind | concatenation lambda body                             | override base-type
   ("router"       ".*"          (:prefix "/router" "." :jsext))
   ("^route$"      "source"      (:prefix "/routes/" :class "." :jsext)                  "route")
   ("model"        "source"      (:prefix "/models/" :class "." :jsext))
   ("view"         "source"      (:prefix "/views/" :class "." :jsext))
   ("component"    "source"      (:prefix "/components/" :class "." :jsext))
   ("controller"   "source"      (:prefix "/controllers/" :class "." :jsext))
   ("mixin"        "source"      (:prefix "/mixins/" :class "." :jsext))
   ("initializer"  "source"      (:prefix "/initializers/" :class "." :jsext))
   ("util"         "source"      (:prefix "/utils/" :class "." :jsext))
   ("service"      "source"      (:prefix "/services/" :class "." :jsext))
   ("component"    "template"    (:prefix "/templates/components/" :class "." :hbext))
   ("template"     ".*"          (:prefix "/templates/" :class "." :hbext))
   (".*"           "template"    (:prefix "/templates/" :class "." :hbext)               "template")
   ;; END contains the definition of each matcher
   )

(ember--define-matchers pod
   ;; BEGIN contains the definition for each matcher
   ;; the first two columns are a regexp, the rest is executed as code
   ;; base-type  | target-kind | concatenation lambda body                               | override base-type
   ("router"       ".*"          (:prefix "/router" "." :jsext))
   ("^route$"      "source"      (:prefix "/" :class "/route" "." :jsext)                  "route")
   ("model"        "source"      (:prefix "/" :class "/model" "." :jsext))
   ("view"         "source"      (:prefix "/" :class "/view" "." :jsext))
   ("controller"   "source"      (:prefix "/" :class "/controller" "." :jsext))
   ("service"      "source"      (:prefix "/" :class "/service" "." :jsext))
   ("component"    "source"      (:prefix "/components/" :class "/component" "." :jsext))
   ("mixin"        "source"      (:prefix "/mixins/" :class "." :jsext))
   ("initializer"  "source"      (:prefix "/initializers/" :class "." :jsext))
   ("util"         "source"      (:prefix "/utils/" :class "." :jsext))
   ("service"      "source"      (:prefix "/services/" :class "." :jsext))
   ("component"    "template"    (:prefix "/components/" :class "/template" "." :hbext))
   ("template"     "source"      (:prefix "/" :class "/template" "." :hbext))
   (".*"           "template"    (:prefix "/" :class "/template" "." :hbext)               "template")
   ;; END contains the definition of each matcher
   )

(defun ember--current-matcher-templates ()
  "Returns the contents of the matcher templates given the current
POD setting."
  (gethash (ember--get-matcher-map-name) *ember--matcher-templates*))

(defun ember--matcher-partial-fill (matcher-template &rest options)
  "Fills in the parts of MATCHER-TEMPLATE which could be filled in
with the supplied OPTIONS.

OPTIONS is expected to be a plist containing the keywords in which
the :prefix keyword is required and :extension and :class are
optional."
  (cl-loop for item in matcher-template
        for substitution = (cl-getf options item)
        if substitution
        collect substitution
        else
        collect item))

(defun ember--matcher-relative-path (matcher-template &rest options)
  "Constructs the relative path for MATCHER-TEMPLATE, given the
options in OPTIONS.

OPTIONS should be an alist containing the keywords :PREFIX, :CLASS
and :EXTENSION.  Some matchers may not require all to be supplied."
  (apply #'concat
         (cl-loop for item in
               (apply #'ember--matcher-partial-fill matcher-template options)
               if (stringp item) collect item
               else collect "")))

(defun ember--matcher-matches-p (matcher base-type target-kind)
  "Returns non-nil iff MATCHER matches BASE-TYPE and TARGET-KIND."
  (cl-destructuring-bind (base-type-regexp target-kind-regexp)
      matcher
    (and (string-match base-type-regexp base-type)
         (string-match target-kind-regexp target-kind))))

(defun ember--matcher-templates-for (base-type target-kind)
  "Returns the matcher templates which match BASE-TYPE and
TARGET-KIND in the order in which the matchers have been
defined."
  (cl-loop for (base-type-regexp target-kind-regexp matcher-template)
        in (ember--current-matcher-templates)
        if (and (string-match base-type-regexp (or base-type ""))
                (string-match target-kind-regexp (or target-kind "")))
        collect matcher-template))

(defun ember--matchers-for (base-type target-kind)
  "Similar to ember--matcher-templates-for, but returning the the
whole matcher"
  (cl-loop for matcher in (ember--current-matcher-templates)
        for (base-type-regexp target-kind-regexp matcher-template) = matcher
        if (and (string-match base-type-regexp (or base-type ""))
                (string-match target-kind-regexp (or target-kind "")))
        collect matcher))

(defun ember--matcher-hbs-template-p (matcher-template)
  "Returns non-nil iff the matcher-template has a handlebars-extension"
  (cl-find :hbext matcher-template))

(defun ember--matcher-js-template-p (matcher-template)
  "Returns non-nil iff the matcher-template has a javascript-extension"
  (cl-find :jsext matcher-template))

(defun ember--matcher-template-map-extensions (matcher-template)
  "Returns a new matcher-template for each of the file-types which fit
the matcher-template"
  (cond ((ember--matcher-js-template-p matcher-template)
         (cl-loop for ext in ember-script-file-types collect
               (ember--matcher-partial-fill matcher-template :jsext ext)))
        ((ember--matcher-hbs-template-p matcher-template)
         (cl-loop for ext in ember-template-file-types collect
               (ember--matcher-partial-fill matcher-template :hbext ext)))
        (t (list matcher-template))))

(defun ember--matcher-template-map-prefixes (matcher-template)
  "Returns a new matcher-template for each prefix"
  (list (ember--matcher-partial-fill matcher-template :prefix "app")
        (ember--matcher-partial-fill matcher-template :prefix "addon")))

(defun ember--regex-escape-matcher-template (matcher-template)
  "Returns the same matcher but in which the components can be used in
a regular expression.  The most common regex patterns will have been
replaced."
  (let ((symbols-to-replace (list "." "+" "*")))
    (cl-loop for char in symbols-to-replace
          do (setf matcher-template
                   (cl-loop for component in matcher-template
                         if (stringp component)
                         collect (replace-regexp-in-string (concat "\\" char)
                                                           (concat "\\\\" char)
                                                           component)
                         else
                         collect component)))
    matcher-template))

(defun ember--matcher-matches-file-p (matcher relative-path)
  "Returns non-nil iff MATCHER matches RELATIVE-PATH.
If this returns non-nil, a PLIST is returned which maps the variables in the
template to their corresponding values in RELATIVE-PATH."
  (let ((matcher-template (ember--get-matcher-template matcher)))
    (let ((component-symbols (cl-loop for component in matcher-template
                                   if (symbolp component)
                                   collect component)))
      (let ((template-regex
             (concat
              "^"
              (apply #'ember--matcher-relative-path
                     (ember--regex-escape-matcher-template matcher-template)
                     (cl-loop for symbol in component-symbols append
                              (let ((regex (if (eq symbol :class)
                                               "\\(.+\\)"
                                               "\\([^/]+\\)")))
                                (list symbol regex))))
              "$")))
        (save-match-data
          (when (string-match template-regex relative-path)
            (cl-loop for component-symbol in component-symbols
                  for index from 1
                  append
                  (list component-symbol (match-string index relative-path)))))))))

(defun ember--relative-ember-source-path (base-prefix base-class base-type target-kind)
  "Supplies a list of plausible paths to an ember source file given
its core components.  The paths are returned as a list of strings,
starting from the app's root.

Sources are specified in ember by a few orthogonal factors:
- BASE-CLASS :: The base class of the element we're talking about.
    For instance:
      - A UserRoute would have a base class of User.
      - A UserModel would have a base class of User.
      - A LoginRoute would have a base class of Login.
- BASE-TYPE :: The type of the class we're talking about.
    For instance:
      - A UserRoute would have a base type of Route.
      - A UserModel would have a base type of Model.
      - A LoginRoute would have a base type of Route.
      - The template of a UserRoute would be app/templates/user.hbs
        but this could also be specified as a base-type of 'user'
        and a target-kind of 'template'.
    Possible values are:
      - router
      - route
      - model
      - controller
      - view
      - component
      - template
      - index (the index template) ;; !the index no longer exists
      - (blank)
- TARGET-KIND :: The target kind is the kind of source file you
    expect to receive.  This is either 'source', 'template', or blank.
    For instance:
      - The coffeescript file for a UserRoute would be 'source'
      - The handlebars file for a UserController would be 'template'
      - The UserComponent's handlebars file would be 'template'
      - The UserComponent's coffeescript file would be 'source'
    Possible values are:
      - template
      - source
      - (blank)"
  (let ((templates (ember--matcher-templates-for base-type target-kind)))
    (when templates
      (mapcar #'ember--matcher-relative-path
              (ember--matcher-template-map-extensions
               (ember--matcher-partial-fill (cl-first templates)
                                            :prefix base-prefix
                                            :class base-class
                                            :base-type base-type
                                            :target-kind target-kind))))))

(defun ember--current-project-root ()
  "Returns the root folder of the current ember project."
  ;; for the current implementation this basically walks up the tree until
  ;; it sees an app folder and assumes the folder containing the app folder
  ;; is the root of the ember project.
  (ember--file-project-root (or load-file-name buffer-file-name default-directory)))

(defun ember--file-project-root (file)
  (locate-dominating-file file ".git"))

(defun ember--relative-file-components (file)
  "Returns a list containing the components which make up this ember source
file.

The components are defined in `ember--relative-ember-source-path'.  This function
returns the base-class, the base-type and the target-kind of the current
file."
  (let ((components-and-matcher
         (cl-loop for matcher in (ember--current-matcher-templates)
               for components = (ember--matcher-matches-file-p matcher file)
               if components
               return (list components matcher))))
    (when components-and-matcher
      (cl-destructuring-bind (components matcher) components-and-matcher
        (let ((base-class (cl-getf components :class))
              (base-type  (cl-fourth matcher))
              (base-prefix (cl-getf components :prefix))
              (target-kind (cond
                            ((cl-find (cl-getf components :jsext) ember-script-file-types :test #'equal)
                             "source")
                            ((cl-find (cl-getf components :hbext) ember-template-file-types :test #'equal)
                             "template"))))
          (list base-prefix base-class base-type target-kind))))))

(defun ember--file-relative-to-root (file)
  "Returns the pathname of FILE relative to the current project's
root."
  (file-relative-name file (ember--file-project-root file)))

(defun ember--current-file-components ()
  "Returns a list containing the components which make up this
ember source file."
  (or (ember--relative-file-components
       (ember--file-relative-to-root (or load-file-name buffer-file-name default-directory)))
      (list nil nil nil nil)))

(cl-defun ember-open-file-by-type (type &optional (assume-js t))
  "Opens an ember file for TYPE with all base values assumed from
the currently open file.

ASSUME-JS is an override.  If this is true, it is assumed that a
javascript (or coffeescript) source file should be opened."
  (cl-destructuring-bind (base-prefix base-class base-type target-kind)
      (ember--current-file-components)
    (let ((new-target-kind (if assume-js "source" target-kind)))
      (if (and (equal type base-type)
               (equal target-kind new-target-kind))
          (ember--select-file-by-type-and-kind (concat "Open " type ": ") base-type new-target-kind)
        (ember-generic-open-file base-prefix base-class type new-target-kind)))))

(defun ember-open-file-by-kind (kind)
  "Opens an ember file for KIND.

Kind should be one of \"template\" or \"source\"."
  (cl-destructuring-bind (base-prefix base-class base-type target-kind)
      (ember--current-file-components)
    (if (equal kind target-kind)
        (ember--select-file-by-type-and-kind (concat "Open " base-type ": ") base-type kind)
      (ember-generic-open-file base-prefix base-class base-type kind))))

(defun ember--get-matcher-template (matcher)
  "Returns the matcher template for MATCHER."
  (cl-third matcher))

(defmacro ember--appendf (list-location appended-list)
  "Appends APPENDED-LIST to the list on LIST-LOCATION and stores
the resulting list in LIST-LOCATION."
  `(setf ,list-location (append ,list-location ,appended-list)))

(defun ember--list-files-by-type-and-kind (base-type target-kind)
  "List files in DIRECTORY and in its sub-directories.

Returns files that match the regular expression MATCH but ignore
files and directories that match IGNORE (IGNORE is tested before
MATCH. Recurse only to depth MAXDEPTH. Does not recurse if
MAXDEPTH is zero or negative."
  (let ((matchers (ember--matchers-for base-type target-kind))
        (walk-dirs (cl-loop for prefix in '("app" "addon")
                            for dir = (concat (ember--current-project-root) prefix)
                            if (file-directory-p dir)
                            collect dir))
        matching-files)
    (cl-flet ((walk-directory
               (dir)
               (dolist (f (directory-files dir t "[A-Za-z]"))
                 (cond ((file-regular-p f)
                        (let ((relative-file (ember--file-relative-to-root f)))
                          (when (cl-some (lambda (m) (ember--matcher-matches-file-p m relative-file)) matchers)
                            (push f matching-files))))
                       ((file-directory-p f)
                        (push f walk-dirs))))))
      (while walk-dirs
        (let ((walk-now walk-dirs))
          (setf walk-dirs nil)
          (dolist (dir walk-now)
            (walk-directory dir)))))
    (setf matching-files (mapcar #'ember--file-relative-to-root matching-files))
    (cl-remove-if #'ember--temporary-file-p matching-files)))

(defun ember--last-char (string)
  "Returns the last character of STRING."
  (string (elt string (1- (length string)))))

(defun ember--temporary-file-p (filename)
  "Returns non-nil iff FILENAME is a temporary file."
  (message (format "checking filename %s" filename))
  (or (equal (ember--last-char filename) "~")
      (equal (string (elt filename 0)) "#")
      (and (>= (length filename) 2)
           (equal (string (elt filename 0)) ".")
           (equal (string (elt filename 1)) "#"))))

(defun ember--completing-read (question matches)
  "A smarter completing-read which poses QUESTION with matches being MATCHES.
This replacement uses a completion system according to
`ember-completion-system'."
  (cond
   ((eq ember-completion-system 'ido)
    (ido-completing-read question matches))
   ((and (eq ember-completion-system 'helm)
         (fboundp 'helm-comp-read))
    (helm-comp-read question matches
                    :must-match t))
   (t (completing-read question matches))))

(defun ember--select-file-by-type-and-kind (question base-type target-kind)
  "Lets the user select an ember file based on its kind and type.

- QUESTION is the question which will be asked to the user.
- BASE-TYPE is the type of the resource.
- TARGET-KIND is the kind of the resource."
  (let ((potential-matches (ember--list-files-by-type-and-kind base-type target-kind)))
    (let ((relative-file (ember--completing-read question potential-matches)))
      (when relative-file
        (find-file (concat (ember--current-project-root) relative-file))))))

(defun ember-generic-open-file (base-prefix base-class base-type target-kind)
  "Tries to open the ember file specified by BASE-CLASS, BASE-TYPE and TARGET-KIND.
If no such file was found, it tries to find related files or
requests the user if the file should be created."
  (unless base-class
    (setf base-class ""))
  (let ((ember-root (ember--current-project-root))
        (file-list
         ;; pick the files and their alternatives, so we have a good list
         ;; to search for an existing file.
         (append (ember--relative-ember-source-path base-prefix base-class base-type target-kind)
                 (ember--relative-ember-source-path base-prefix (ember--pluralize-noun base-class) base-type target-kind)
                 (ember--relative-ember-source-path base-prefix (ember--singularize-noun base-class) base-type target-kind))))
    (cl-block found-file
      (cl-loop for relative-file in file-list
               for absolute-file = (concat ember-root relative-file)
               if (file-exists-p absolute-file)
               do
               (find-file absolute-file)
               (cl-return-from found-file absolute-file))
      (when (string= target-kind "template")
        (setf base-type "template"))
      (ember--select-file-by-type-and-kind "Not found, alternatives: " base-type target-kind))))

(defun ember-open-component ()
  "Opens a component file based on the currently opened file."
  (interactive)
  (ember-open-file-by-type "component"))

(defun ember-open-router ()
  "Opens the Router file based on the currently opened file."
  (interactive)
  (ember-open-file-by-type "router"))

(defun ember-open-controller ()
  "Opens an ember Controller file based on the currently opened file."
  (interactive)
  (ember-open-file-by-type "controller"))

(defun ember-open-model ()
  "Opens an ember Model file based on the currently opened file."
  (interactive)
  (ember-open-file-by-type "model"))

(defun ember-open-route ()
  "Opens an ember Route file based on the currently opened file."
  (interactive)
  (ember-open-file-by-type "route"))

(defun ember-open-mixin ()
  "Opens an ember Mixin file based on the currently opened file."
  (interactive)
  (ember-open-file-by-type "mixin"))

(defun ember-open-initializer ()
  "Opens an ember Initializer file based on the currently opened file."
  (interactive)
  (ember-open-file-by-type "initializer"))

(defun ember-open-util ()
  "Opens an ember Utility file based on the currently opened file."
  (interactive)
  (ember-open-file-by-type "util"))

(defun ember-open-service ()
  "Opens an ember Service file based on the currently opened file."
  (interactive)
  (ember-open-file-by-type "service"))

(defun ember-open-template ()
  "Opens an ember Template file based on the currently opened file."
  (interactive)
  (ember-open-file-by-kind "template"))

(defun ember-open-javascript ()
  "Opens an ember Javascript file based on the currently opened file.

This may be handy if you are visiting a template and want to open
the corresponding source."
  (interactive)
  (ember-open-file-by-kind "source"))

(defun ember-open-view ()
  "Opens an ember View file based on the currently opened file."
  (interactive)
  (ember-open-file-by-type "view"))

(defun ember-toggle-addon ()
  "Toggles between the native view and the ember addon view."
  (interactive)
  (cl-destructuring-bind (base-prefix base-class base-type target-kind)
      (ember--current-file-components)
    (let ((new-base-prefix (cond ((string= base-prefix "app")
                                  "addon")
                                 ((string= base-prefix "addon")
                                  "app")
                                 (t (message "Not sure if I'm in app or addon")))))
      (ember-generic-open-file new-base-prefix base-class base-type target-kind))))

;;;;;;;;;;;;;;
;;; Generators

(defun ember--match-by-index (regex string index)
  (save-match-data
    (and (string-match regex string)
         (match-string index string))))

(defun ember-generate (generator kind options)
  "Runs an ember generator."
  (interactive (ember--interactive-generator-options))
  (let ((default-directory (ember--current-project-root)))
    (let ((command (concat ember-command " generate " generator " " kind " " options)))
      (let ((response (shell-command-to-string command)))
        (message response)
        ;; open the first file that was created
        (find-file (concat default-directory "/"
                           (ember--match-by-index "\s+create\s+\\(.*\\)" response 1)))))))

(defun ember-generate-controller (kind options)
  "Generates a controller."
  (interactive (ember--interactive-generator-options "controller"))
  (ember-generate "controller" kind options))

(defun ember-generate-component (kind options)
  "Generates a component."
  (interactive (ember--interactive-generator-options "component"))
  (ember-generate "component" kind options))

(defun ember-generate-model (kind options)
  "Generates a model."
  (interactive (ember--interactive-generator-options "model"))
  (ember-generate "model" kind options))

(defun ember-generate-route (kind options)
  "Generates a route."
  (interactive (ember--interactive-generator-options "route"))
  (ember-generate "route" kind options))

(defun ember-generate-mixin (kind options)
  "Generates a mixin."
  (interactive (ember--interactive-generator-options "mixin"))
  (ember-generate "mixin" kind options))

(defun ember-generate-initializer (kind options)
  "Generates a initializer."
  (interactive (ember--interactive-generator-options "initializer"))
  (ember-generate "initializer" kind options))

(defun ember-generate-util (kind options)
  "Generates a utility."
  (interactive (ember--interactive-generator-options "util"))
  (ember-generate "util" kind options))

(defun ember-generate-service (kind options)
  "Generates a service."
  (interactive (ember--interactive-generator-options "service"))
  (ember-generate "service" kind options))

(defun ember-generate-template (kind options)
  "Generates a template."
  (interactive (ember--interactive-generator-options "template"))
  (ember-generate "template" kind options))

(defun ember-generate-view (kind options)
  "Generates a view."
  (interactive (ember--interactive-generator-options "view"))
  (ember-generate "view" kind options))

(defun ember--generators ()
  "Returns a list of all generators."
  (split-string
   (shell-command-to-string
    (concat ember-command
            " help generate | grep -P '      (.*)' | grep -P -o '[a-z\\-]+' | sort | uniq"))))

(defun ember--interactive-generator-options
  (&optional supplied-generator supplied-kind destroy-p)
  "Generates a function interactive statement which ensures the
arguments for the generator are known.

The interative statement will try to find all unknown values from
`ember--current-file-compononents'.

If a value was supplied to this macro directly, then that value
will be assumed to be the final value.

If destroy-p is true, the user will be informed the requests are
regarding the destruction process, rather than regarding the
generate-process.

The user will be queried for all values which weren't supplied
and which could not be found by `ember--current-file-components'
or if the user has supplied a prefix-argument.  In the case of a
prefix argument all values not supplied by SUPPLIED-GENERATOR or
SUPPLIED-KIND will be queried with the default being the value
found by `ember--current-file-components'."
  (cl-destructuring-bind (current-base-prefix current-base-class current-base-kind current-target-kind)
      ;; fetch current values from current-file-components
      (condition-case err
          (ember--current-file-components)
        ('error (list nil nil nil nil)))
    ;; ask the user to override
    (cl-destructuring-bind (new-generator new-kind new-options)
        (if current-prefix-arg
            (let* ((generator (or supplied-generator current-base-kind
                                  (ember--completing-read "Generator: " (ember--generators))))
                   (kind (or supplied-kind current-base-class
                             (read-string (concat (if destroy-p "Destroying" "Generating") " "
                                                  generator " for kind: ")))))
              (list generator kind ""))
          (let* ((generator (or supplied-generator
                                (ember--completing-read "Generator: " (ember--generators))))
                 (kind (or supplied-kind (read-string (concat (if destroy-p "Destroying" "Generating") " "
                                                              generator " for kind: ")
                                                      current-base-class)))
                 (options (read-string "Options: " )))
            (list generator kind options)))
      ;; figure out which values we should return
      (let ((result (list new-options)))
        (unless supplied-kind
          (push new-kind result))
        (unless supplied-generator
          (push new-generator result))
        result))))


;;; destroy
(defun ember-destroy (generator kind options)
  "Runs an ember generator."
  (interactive (ember--interactive-generator-options nil nil t))
  (let ((default-directory (ember--current-project-root)))
    (let ((response
           (shell-command-to-string (concat ember-command " destroy " generator " " kind " " options))))
      (message response)
      ;; open the first file that was created
      (find-file (concat default-directory "/"
                         (ember--match-by-index "\s+create\s+\\(.*\\)" response 1))))))

(defun ember-destroy-controller (kind options)
  "Destroys a controller."
  (interactive (ember--interactive-generator-options "controller" nil t))
  (ember-destroy "controller" kind options))

(defun ember-destroy-component (kind options)
  "Destroys a component."
  (interactive (ember--interactive-generator-options "component" nil t))
  (ember-destroy "component" kind options))

(defun ember-destroy-model (kind options)
  "Destroys a model."
  (interactive (ember--interactive-generator-options "model" nil t))
  (ember-destroy "model" kind options))

(defun ember-destroy-route (kind options)
  "Destroys a route."
  (interactive (ember--interactive-generator-options "route" nil t))
  (ember-destroy "route" kind options))

(defun ember-destroy-mixin (kind options)
  "Destroys a mixin."
  (interactive (ember--interactive-generator-options "mixin" nil t))
  (ember-destroy "mixin" kind options))

(defun ember-destroy-initializer (kind options)
  "Destroys a initializer."
  (interactive (ember--interactive-generator-options "initializer" nil t))
  (ember-destroy "initializer" kind options))

(defun ember-destroy-util (kind options)
  "Destroys a utility."
  (interactive (ember--interactive-generator-options "util" nil t))
  (ember-destroy "util" kind options))

(defun ember-destroy-service (kind options)
  "Destroys a service."
  (interactive (ember--interactive-generator-options "service" nil t))
  (ember-destroy "service" kind options))

(defun ember-destroy-template (kind options)
  "Destroys a template."
  (interactive (ember--interactive-generator-options "template" nil t))
  (ember-destroy "template" kind options))

(defun ember-destroy-view (kind options)
  "Destroys a view."
  (interactive (ember--interactive-generator-options "view" nil t))
  (ember-destroy "view" kind options))


;;;;;;;;;;;;;;;;;;;;;
;;; Compilation modes

(defcustom ember-serve-command
  "ember serve"
  "Default command for running ember serve with `ember-serve-or-display'."
  :type 'string
  :group 'ember)

(defcustom ember-build-command
  "ember build --environment=production"
  "Default command for running ember build with `ember-build'."
  :type 'string
  :group 'ember)

(defcustom ember-test-command
  "ember test"
  "Default command for running ember test with `ember-test'."
  :type 'string
  :group 'ember)

(defvar ember--serve-history nil)
(defvar ember--build-history nil)
(defvar ember--test-history nil)

(defvar ember--test-regexps
  '((ember-test-ok
     "^\\(ok\\)" nil nil nil 0 nil
     (1 compilation-info-face))
    (ember-test-notok
     "^\\(not ok\\)" nil nil nil 2 nil
     (1 compilation-error-face))))

(defvar ember--error-regexps
  '((ember-error
     "File: \\(.+\\)\n.*?[lL]ine \\([0-9]+\\).*"
     ember--resolve-broken-error-filename 2 nil 2 nil
     (1 compilation-error-face))
    (ember-error-2
     "File: \\(.+\\)\n.* \\([0-9]+\\):\\([0-9]+\\)"
     ember--resolve-broken-error-filename 2 3 2 nil
     (1 compilation-error-face))
    (ember-jshint
     "\\([^, \n]+\\): line \\([0-9]+\\), col \\([0-9]+\\), .*"
     ember--resolve-error-filename 2 3 1 nil
     (1 compilation-warning-face))
    (ember-babel
     "\\(?:SyntaxError: \\)?\\([^, \n]+\\): [^(\n]+ (\\([0-9]+\\):\\([0-9]+\\))"
     ember--resolve-broken-error-filename 2 3 2 nil
     (1 compilation-error-face))))

(defun ember--resolve-error-filename ()
  "Resolves a filename that is relative to the app directory."
  (expand-file-name (match-string 1)
                    (concat default-directory "app")))

(defun ember--resolve-broken-error-filename ()
  "Resolves a filename that does not correspond exactly to the real path.
For example, if you have a project named foo, the paths look like
/foo/templates/application.hbs when the correct path is
/foo/app/templates/application.hbs."
  (let* ((filename (match-string 1))
         (broken-name (file-name-directory filename))
         new-filename)
    (while (and
            (setq new-filename
                  (file-name-directory (directory-file-name broken-name)))
            (not (eq new-filename "/")))
      (setq broken-name new-filename))
    (expand-file-name (file-relative-name filename broken-name)
                      (concat default-directory "app"))))

(defun ember--load-error-regexps (regexps)
  "Load compilation error regexps from REGEXPS."
  (make-local-variable 'compilation-error-regexp-alist-alist)
  (make-local-variable 'compilation-error-regexp-alist)
  (dolist
      (regexp regexps)
    (add-to-list 'compilation-error-regexp-alist-alist regexp)
    (add-to-list 'compilation-error-regexp-alist (car regexp))))

(defun ember-serve-or-display (command)
  "Run ember serve using COMMAND, or switch to buffer if already running."
  (interactive "i")
  (let* ((buffer-name "*ember-serve*")
         (buffer (get-buffer buffer-name)))
    (if (and buffer
             (get-buffer-process buffer))
        (display-buffer buffer-name)
      (let ((command (or command
                         (read-shell-command "Serve command: "
                                             ember-serve-command
                                             'ember--serve-history)))
            (default-directory (ember--current-project-root)))
        (compilation-start command 'ember-serve-mode)))))

(define-derived-mode ember-serve-mode compilation-mode "Serving"
  "Mode for running ember serve."
  (ember--load-error-regexps ember--error-regexps)
  (add-hook 'compilation-filter-hook
            (lambda ()
              (unless (get-buffer-window "*ember-serve*" 'visible)
                (save-match-data
                  (save-excursion
                    (let ((end (point)))
                      (dolist (regexp (append
                                       '("^file changed .+"
                                         "^Build successful .+"
                                         "^Error: .+")
                                       (mapcar 'cadr ember--error-regexps)))
                        (goto-char compilation-filter-start)
                        (when (re-search-forward regexp end t)
                          (message "Ember serve: %s" (match-string 0)))))))))
            nil
            t)
  (set (make-local-variable 'compilation-scroll-output) t)
  (add-to-list (make-local-variable 'compilation-finish-functions)
               (lambda (buffer result)
                 (unless (get-buffer-window "*ember-serve*" 'visible)
                   (message "Ember serve exited: %s" result)))))

(defun ember-build (command)
  "Run ember build using COMMAND."
  (interactive (list
                (read-shell-command "Build command: "
                                    ember-build-command
                                    'ember--build-history)))
  (let ((default-directory (ember--current-project-root)))
    (compilation-start command 'ember-build-mode)))

(define-derived-mode ember-build-mode compilation-mode "Building"
  "Mode for running ember build."
  (ember--load-error-regexps ember--error-regexps))

(defun ember-test (command)
  "Run ember test using COMMAND."
  (interactive (list
                (read-shell-command "Test command: "
                                    ember-test-command
                                    'ember--test-history)))
  (let ((default-directory (ember--current-project-root)))
    (compilation-start command 'ember-test-mode)))

(define-derived-mode ember-test-mode compilation-mode "Testing"
  "Mode for running ember test."
  (ember--load-error-regexps (append ember--error-regexps ember--test-regexps)))

;;;;;;;;;;;;;;;
;;; Keybindings

(defvar ember-command-prefix (make-sparse-keymap))

(defvar ember-mode-keymap (make-sparse-keymap)
  "Keymap for ember-mode.")

(define-key ember-command-prefix (kbd "f p") #'ember-open-component)
(define-key ember-command-prefix (kbd "f o") #'ember-open-router)
(define-key ember-command-prefix (kbd "f c") #'ember-open-controller)
(define-key ember-command-prefix (kbd "f m") #'ember-open-model)
(define-key ember-command-prefix (kbd "f r") #'ember-open-route)
(define-key ember-command-prefix (kbd "f t") #'ember-open-template)
(define-key ember-command-prefix (kbd "f j") #'ember-open-javascript)
(define-key ember-command-prefix (kbd "f v") #'ember-open-view)
(define-key ember-command-prefix (kbd "f x") #'ember-open-mixin)
(define-key ember-command-prefix (kbd "f i") #'ember-open-initializer)
(define-key ember-command-prefix (kbd "f u") #'ember-open-util)
(define-key ember-command-prefix (kbd "f s") #'ember-open-service)
(define-key ember-command-prefix (kbd "f a") #'ember-toggle-addon)

(define-key ember-command-prefix (kbd "g g") #'ember-generate)
(define-key ember-command-prefix (kbd "g p") #'ember-generate-component)
(define-key ember-command-prefix (kbd "g c") #'ember-generate-controller)
(define-key ember-command-prefix (kbd "g m") #'ember-generate-model)
(define-key ember-command-prefix (kbd "g r") #'ember-generate-route)
(define-key ember-command-prefix (kbd "g t") #'ember-generate-template)
(define-key ember-command-prefix (kbd "g v") #'ember-generate-view)
(define-key ember-command-prefix (kbd "g x") #'ember-generate-mixin)
(define-key ember-command-prefix (kbd "g i") #'ember-generate-initializer)
(define-key ember-command-prefix (kbd "g u") #'ember-generate-util)
(define-key ember-command-prefix (kbd "g s") #'ember-generate-service)

(define-key ember-command-prefix (kbd "d g") #'ember-destroy)
(define-key ember-command-prefix (kbd "d p") #'ember-destroy-component)
(define-key ember-command-prefix (kbd "d c") #'ember-destroy-controller)
(define-key ember-command-prefix (kbd "d m") #'ember-destroy-model)
(define-key ember-command-prefix (kbd "d r") #'ember-destroy-route)
(define-key ember-command-prefix (kbd "d t") #'ember-destroy-template)
(define-key ember-command-prefix (kbd "d v") #'ember-destroy-view)
(define-key ember-command-prefix (kbd "d x") #'ember-destroy-mixin)
(define-key ember-command-prefix (kbd "d i") #'ember-destroy-initializer)
(define-key ember-command-prefix (kbd "d u") #'ember-destroy-util)
(define-key ember-command-prefix (kbd "d s") #'ember-destroy-service)

(define-key ember-command-prefix (kbd "r b") 'ember-build)
(define-key ember-command-prefix (kbd "r s") 'ember-serve-or-display)
(define-key ember-command-prefix (kbd "r t") 'ember-test)

(fset 'ember-command-prefix ember-command-prefix)

(define-key ember-mode-keymap ember-keymap-prefix 'ember-command-prefix)

;;;###autoload
(define-minor-mode ember-mode
  "Mode for navigating around ember-cli applications."
  nil " [EM]" ember-mode-keymap)

(provide 'ember-mode)
;;; ember-mode.el ends here
