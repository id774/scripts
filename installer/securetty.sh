#!/bin/sh

########################################################################
# securetty.sh: Script to Clear /etc/securetty for Unrestricted Root Access
#
#  Description:
#  This script checks if /etc/securetty is a regular file and clears its content
#  to enable root login from any TTY device. This might be necessary for certain
#  system administration tasks or policy changes.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.4 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.3 2025-03-22
#       Unify usage information by extracting help text from header comments.
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
#  Warning:
#  Using this script can decrease system security by allowing root access from any
#  terminal. It should only be used when absolutely necessary and in secure environments.
#  This script does nothing if /etc/securetty is a directory, which might be the case
#  in some systems like macOS.
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
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Function to check the type of /etc/securetty and take action
clear_securetty() {
    if [ -f /etc/securetty ]; then
        sudo sh -c ": > /etc/securetty"
        echo "[INFO] Setup completed."
    elif [ -d /etc/securetty ]; then
        echo "[ERROR] /etc/securetty is a directory, no changes were made." >&2
        exit 1
    else
        echo "[ERROR] /etc/securetty does not exist as a file or directory." >&2
        exit 1
    fi
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    check_commands sudo sh
    check_sudo
    clear_securetty
}

# Execute main function
main "$@"
