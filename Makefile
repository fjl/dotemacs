all:
	emacs -q --batch -l lisp/init-bootstrap.el -f make-init-autoloads -eval '(batch-byte-recompile-directory 0)' .

clean:
	find . -name "*.elc" -print -delete
	rm -f lisp/init-autoloads.el


