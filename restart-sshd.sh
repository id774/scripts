#!/bin/sh

########################################################################
# restart-sshd.sh: Restart SSH Daemon
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
#  v1.5 2025-03-13
#       Improved error handling.
#  v1.4 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v1.3 2025-01-03
#       Added existence check for launchctl command in macOS environment
#       to improve error handling and reliability.
#  v1.2 2023-12-23
#       Refactored for POSIX compliance. Replaced Bash-specific syntax
#       with POSIX standard commands and structures. Enhanced portability
#       and compatibility across different UNIX-like systems.
#  v1.1 2023-12-06
#       Improved system environment check and added command/file existence verification.
#  v1.0 2022-09-13
#       Initial release.
#
#  Usage:
#  ./restart-sshd.sh
#
########################################################################

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Check for necessary commands and files before execution
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

restart_macos_sshd() {
    SSH_PLIST="/System/Library/LaunchDaemons/ssh.plist"
    if ! command_exists launchctl; then
        echo "launchctl command not found. Unable to restart SSH on macOS." >&2
        exit 1
    fi

    if [ -f "$SSH_PLIST" ]; then
        check_sudo
        sudo launchctl unload -w "$SSH_PLIST" && sudo launchctl load -w "$SSH_PLIST"
    else
        echo "SSH plist file not found: $SSH_PLIST" >&2
        exit 1
    fi
}

restart_linux_sshd() {
    if command_exists systemctl; then
        check_sudo
        sudo systemctl restart ssh.service
    else
        echo "systemctl not found. Unable to restart SSH." >&2
        exit 1
    fi
}

# Determine the operating system and restart SSH accordingly
UNAME=$(uname)
case $UNAME in
    Darwin*)
        restart_macos_sshd
        ;;
    *)
        restart_linux_sshd
        ;;
esac

