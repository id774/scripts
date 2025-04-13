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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.7 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.6 2025-03-17
#       Encapsulated all logic into functions and introduced main function.
#  v1.5 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
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
#      ./restart-sshd.sh
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

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Check for necessary commands and files before execution
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Restart SSH daemon on macOS
restart_macos_sshd() {
    SSH_PLIST="/System/Library/LaunchDaemons/ssh.plist"
    if ! command_exists launchctl; then
        echo "[ERROR] launchctl command not found. Unable to restart SSH on macOS." >&2
        exit 1
    fi

    if [ -f "$SSH_PLIST" ]; then
        sudo launchctl unload -w "$SSH_PLIST" && sudo launchctl load -w "$SSH_PLIST"
    else
        echo "[ERROR] SSH plist file not found: $SSH_PLIST" >&2
        exit 1
    fi
}

# Restart SSH daemon on Linux
restart_linux_sshd() {
    if command_exists systemctl; then
        sudo systemctl restart ssh.service
    else
        echo "[ERROR] systemctl not found. Unable to restart SSH." >&2
        exit 1
    fi
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_sudo

    UNAME=$(uname)
    case $UNAME in
        Darwin*)
            restart_macos_sshd
            ;;
        *)
            restart_linux_sshd
            ;;
    esac
}

# Execute main function
main "$@"
