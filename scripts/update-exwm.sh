#!/bin/bash

set -e

function update-subtree () {
    if ! git remote get-url $1-upstream 2>/dev/null; then
        git remote add -f $1-upstream https://github.com/ch11ng/$1
    fi
    [ -d elpa/$1 ] && git rm -qr elpa/$1
    git read-tree --prefix=elpa/$1/ -u $1-upstream/master
    echo "subtree set up at elpa/$1"
}

update-subtree exwm
update-subtree xelb
update-subtree exim
