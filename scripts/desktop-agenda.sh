#!/bin/bash

set -e

PATH="/usr/local/bin:$PATH"
BASEDIR="$(dirname $0)/.."

# update desktop-agenda.txt
emacs -batch -q \
      -l "$BASEDIR/init.el" \
      -l "$BASEDIR/lisp/init-org.el" \
      -eval '(org-store-agenda-views "D")' \
   &>/dev/null

cat ~/.desktop-agenda.txt
