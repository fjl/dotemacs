all:
	emacs -q --batch -l lisp/init-bootstrap.el -f make-init-autoloads
	emacs -q --batch -l lisp/init-bootstrap.el -eval '(cd "lisp")' -eval '(batch-byte-recompile-directory 0)'
	emacs -q --batch -l lisp/init-bootstrap.el -eval '(cd "elpa")' -eval '(batch-byte-recompile-directory 0)'

clean:
	find . -name "*.elc" -print -delete
	rm -f lisp/init-autoloads.el


