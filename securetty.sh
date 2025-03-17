#!/bin/sh

########################################################################
# securetty.sh: Script to Clear /etc/securetty for Unrestricted Root Access
#
#  Description:
#  This script checks if /etc/securetty is a regular file and clears its content
#  to enable root login from any TTY device. This might be necessary for certain
#  system administration tasks or policy changes.
#
#  Warning:
#  Using this script can decrease system security by allowing root access from any
#  terminal. It should only be used when absolutely necessary and in secure environments.
#  This script does nothing if /etc/securetty is a directory, which might be the case
#  in some systems like macOS.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2025-03-17
#       Encapsulated all logic in functions and introduced main function.
#  v1.1 2023-11-30
#       Added check to ensure /etc/securetty is a file before clearing it.
#  v1.0 2012-05-21
#       Initial release. Script to clear /etc/securetty for unrestricted root access.
#
#  Usage:
#  Run the script without any arguments:
#      ./securetty.sh
#
########################################################################

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
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if [ "$SUDO" = "sudo" ] && ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access or specify 'no-sudo'." >&2
        exit 1
    fi
}

# Function to check the type of /etc/securetty and take action
clear_securetty() {
    if [ -f /etc/securetty ]; then
        sudo sh -c ": > /etc/securetty"
    elif [ -d /etc/securetty ]; then
        echo "/etc/securetty is a directory, no changes were made." >&2
        exit 1
    else
        echo "/etc/securetty does not exist as a file or directory." >&2
        exit 1
    fi
}

# Main function
main() {
    check_system
    check_commands sudo sh
    check_sudo
    clear_securetty
}

# Execute main function
main "$@"
