#!/bin/sh

export DISPLAY=:99

start_xvfb() {
    sudo Xvfb $DISPLAY -screen 0 1024x768x24 2>&1 >> $TMP/xvfb.log &
}

start_fluxbox() {
    sudo fluxbox 2>&1 >> $TMP/window-manager.log &
}

start_xvfb_and_fluxbox() {
    start_xvfb
    sleep 10
    start_fluxbox
}

start_vnc() {
    x11vnc -display $DISPLAY -loop -bg -nopw -listen localhost -xkb 2>&1 >> $TMP/vnc.log &
}

main() {
    start_xvfb_and_fluxbox
    start_vnc
}

main
