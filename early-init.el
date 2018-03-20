;; Prevent loading packages twice. All packages are already activated in
;; lisp/init-bootstrap.el. package.el will do it again after the init file is loaded
;; unless disabled.
(setq package-enable-at-startup nil)