#!/bin/bash

########################################################################
# restart_sshd.sh: Restart SSH Daemon
#
#  Description:
#  This script restarts the SSH daemon. It supports both macOS (using
#  launchctl) and Linux systems (using systemctl).
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-06
#       Improved system environment check and added command/file existence verification.
#  v1.0 2022-09-13
#       Initial release.
#
#  Usage:
#  ./restart_sshd.sh
#
########################################################################

# Check for necessary commands and files before execution
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

restart_macos_sshd() {
    SSH_PLIST="/System/Library/LaunchDaemons/ssh.plist"
    if [ -f "$SSH_PLIST" ]; then
        sudo launchctl unload -w "$SSH_PLIST" && sudo launchctl load -w "$SSH_PLIST"
    else
        echo "SSH plist file not found: $SSH_PLIST"
        exit 1
    fi
}

restart_linux_sshd() {
    if command_exists systemctl; then
        sudo systemctl restart ssh.service
    else
        echo "systemctl not found. Unable to restart SSH."
        exit 1
    fi
}

# Determine the operating system and restart SSH accordingly
case $OSTYPE in
    *darwin*)
        restart_macos_sshd
        ;;
    *)
        restart_linux_sshd
        ;;
esac

