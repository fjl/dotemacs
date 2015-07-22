all:
	emacs -q --batch -l lisp/init-bootstrap.el -l lisp/init-commands.el -f init-compile

clean:
	find . -name "*.elc" -print -delete
	rm -f lisp/init-autoloads.el


