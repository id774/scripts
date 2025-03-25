#!/bin/sh

########################################################################
# vmplayer-start.sh: VMware Player Startup Script
#
#  Description:
#  This script starts VMware Player in a virtual X server environment.
#  It uses xvfb, fluxbox, x11vnc, and vmplayer to set up a virtual desktop
#  accessible via VNC.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v0.4 2025-03-25
#       Rename start-vmplayer.sh to vmplayer-start.sh.
#       Refactor main function to perform system and command checks.
#  v0.3 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v0.2 2023-12-20
#       Replaced 'which' with 'command -v' for command existence check.
#       Updated script header for consistency.
#  v0.1 2011-11-07
#       First release.
#
#  Usage:
#      ./vmplayer-start.sh
#
#  Notes:
#  This script requires xvfb, fluxbox (non-free), x11vnc (non-free),
#  and vmplayer (non-free) packages.
#  Start with the privileges of the user.
#
#  To connect from a client, use SSH port forwarding:
#      ssh servername -L5900:localhost:5900
#
#  Then, connect to localhost:5900 using xtightvncviewer.
#  Kill the VNC and fluxbox for safety after starting VM.
#
########################################################################

# Display script usage information
usage() {
    awk '
        BEGIN { in_usage = 0 }
        /^#  Usage:/ { in_usage = 1; print substr($0, 4); next }
        /^#{10}/ { if (in_usage) exit }
        in_usage && /^#/ { print substr($0, 4) }
    ' "$0"
    exit 0
}

# Function to check if the system is Linux
check_system() {
    if [ "$(uname -s)" != "Linux" ]; then
        echo "Error: This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Start a virtual X server using Xvfb.
start_xvfb() {
    sudo Xvfb $DISPLAY -screen 0 1024x768x24 2>&1 >> $TMP/xvfb.log &
    sleep 10
}

# Launch the Fluxbox window manager.
start_fluxbox() {
    fluxbox 2>&1 >> $TMP/window-manager.log &
    sleep 5
}

# Start x11vnc server to expose the virtual display over VNC.
start_vnc() {
    x11vnc -display $DISPLAY -loop -bg -nopw -listen localhost -xkb 2>&1 >> $TMP/vnc.log &
    sleep 10
}

# Launch VMware Player twice (for redundancy or multiple instances).
start_vmplayer() {
    vmplayer 2>&1 >> $TMP/vmplayer1.log &
    vmplayer 2>&1 >> $TMP/vmplayer2.log &
    sleep 10
}

# Start all required background processes in sequence.
start_daemon() {
    start_xvfb
    start_fluxbox
    start_vnc
    start_vmplayer
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    check_commands Xvfb fluxbox x11vnc vmplayer

    test -n "$TMP" || export TMP=$HOME/.tmp
    test -n "$1" && export DISPLAY=$1
    test -n "$1" || export DISPLAY=:99

    start_daemon
}

# Execute main function
main "$@"
