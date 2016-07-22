#!/bin/sh

# Setup for EXIM.
export XMODIFIERS=@im=exim
export GTK_IM_MODULE=xim
export QT_IM_MODULE=xim
export CLUTTER_IM_MODULE=xim

emacs -l "$(dirname $0)/../lisp/init-exwm.el"

# Terminate the session when emacs exits.
gnome-session-quit --logout --force
