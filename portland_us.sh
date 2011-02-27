#!/bin/sh

sudo apt-get install xdg-user-dirs-gtk
LANG=C xdg-user-dirs-gtk-update
vim $HOME/.config/user-dirs.dirs
