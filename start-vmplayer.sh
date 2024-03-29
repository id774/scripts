#!/bin/sh

########################################################################
# start-vmplayer.sh: VMware Player Startup Script
#
#  Description:
#  This script starts VMware Player in a virtual X server environment.
#  It uses xvfb, fluxbox, x11vnc, and vmplayer to set up a virtual desktop
#  accessible via VNC.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v0.2 2023-12-20
#       Replaced 'which' with 'command -v' for command existence check.
#       Updated script header for consistency.
#  v0.1 2011-11-07
#       First release.
#
#  Notes:
#  This script requires xvfb, fluxbox (non-free), x11vnc (non-free),
#  and vmplayer (non-free) packages.
#  Start with the privileges of the user.
#  To connect from a client, use SSH port forwarding:
#      $ ssh servername -L5900:localhost:5900
#  Then, connect to localhost:5900 using xtightvncviewer.
#  Kill the VNC and fluxbox for safety after starting VM.
#
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

command -v Xvfb > /dev/null && \
  command -v fluxbox > /dev/null && \
  command -v x11vnc > /dev/null && \
  command -v vmplayer > /dev/null && \
  start_daemon

