#!/bin/sh

# add F14 as an emergency space key, in case xcape fails.
# xcape also needs a keycode for space, so this is convenient
# both ways.
xmodmap -e 'keycode 96 = space'

# make space send Hyper_L
xmodmap -e 'keycode 65 = Hyper_L'

# make Hyper_L a Control modifier
xmodmap -e 'remove mod4 = Hyper_L'
xmodmap -e 'add Control = Hyper_L'

sleep 0.3

exec xcape -e Hyper_L=space
