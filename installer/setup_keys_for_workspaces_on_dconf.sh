#!/bin/sh
#
########################################################################
# Batch Setup Script for dconf-editor
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2020-02-04
#       First.
########################################################################

setup_switch_keys() {
    while [ $# -gt 0 ]
    do
        dconf write /org/gnome/desktop/wm/keybindings/switch-to-workspace-$1 "['<Primary>$1']"
        dconf read /org/gnome/desktop/wm/keybindings/switch-to-workspace-$1
        shift
    done
}

setup_switch_keys 1 2 3 4 5 6 7 8 9

setup_move_keys() {
    while [ $# -gt 0 ]
    do
        dconf write /org/gnome/desktop/wm/keybindings/move-to-workspace-$1 "['<Primary><Alt>$1']"
        dconf read /org/gnome/desktop/wm/keybindings/move-to-workspace-$1
        shift
    done
}

setup_move_keys 1 2 3 4 5 6 7 8 9
