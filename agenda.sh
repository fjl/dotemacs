#!/bin/bash

set -e

# update desktop-agenda.txt
/usr/local/bin/emacs -batch -q \
                     -l ~/.emacs.d/init.el \
                     -l ~/.emacs.d/lisp/init-org.el \
                     -eval '(org-store-agenda-views "D")' \
                     &>/dev/null 

cat ~/.desktop-agenda.txt
