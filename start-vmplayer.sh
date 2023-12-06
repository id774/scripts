#!/bin/sh

########################################################################
# VMware Player startup script
#
# Note:
#  This needs following packages
#    - xvfb
#    - fluxbox (non-free)
#    - x11vnc (non-free)
#    - vmplayer (non-free)
#
#  Start with the privileges of the user.
#
#  To connect from a client, use SSH portfowarding following
#  $ ssh servername -L5900:localhost:5900
#
#  Next, connect localhost:5900 by xtightvncviewer.
#
#  Kill the VNC and fluxbox for safety after starting VM.
#
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2011-11-07
#       First version.
########################################################################

test -n "$TMP" || export TMP=$HOME/.tmp
test -n "$1" && export DISPLAY=$1
test -n "$1" || export DISPLAY=:99

start_xvfb() {
    sudo Xvfb $DISPLAY -screen 0 1024x768x24 2>&1 >> $TMP/xvfb.log &
    sleep 10
}

start_fluxbox() {
    fluxbox 2>&1 >> $TMP/window-manager.log &
    sleep 5
}

start_vnc() {
    x11vnc -display $DISPLAY -loop -bg -nopw -listen localhost -xkb 2>&1 >> $TMP/vnc.log &
    sleep 10
}

start_vmplayer() {
    vmplayer 2>&1 >> $TMP/vmplayer1.log &
    vmplayer 2>&1 >> $TMP/vmplayer2.log &
    sleep 10
}

start_daemon() {
    start_xvfb
    start_fluxbox
    start_vnc
    start_vmplayer
}

which Xvfb > /dev/null && \
  which fluxbox > /dev/null && \
  which x11vnc > /dev/null && \
  which vmplayer > /dev/null && \
  start_daemon
