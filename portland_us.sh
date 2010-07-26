#!/bin/sh

if [ `aptitude search xdg-user-dirs-gtk | awk '/^i/' | wc -l` = 0 ]; then
    sudo aptitude install xdg-user-dirs-gtk
fi

LANG=C xdg-user-dirs-gtk-update
vim $HOME/.config/user-dirs.dirs
