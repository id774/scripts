#!/bin/sh

########################################################################
# setup_securetty.sh: Script to Clear /etc/securetty for Unrestricted Root Access
#
#  Description:
#  This script checks if /etc/securetty is a regular file and clears its content
#  to enable root login from any TTY device. This might be necessary for certain
#  system administration tasks or policy changes.
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run the script without any arguments:
#      ./setup_securetty.sh
#
#  Warning:
#  - Using this script can decrease system security by allowing root access
#    from any terminal. It should only be used when absolutely necessary and
#    in secure environments.
#  - This script does nothing if /etc/securetty is a directory, which might
#    be the case in some systems like macOS.
#  - An absent /etc/securetty is a successful no-op.
#  - An already empty regular /etc/securetty is a successful no-op.
#
#  Requirements:
#  - Linux system.
#  - The invoking user must have sudo privileges.
#
#  Exit Status:
#  0: Success, including absent, directory, or already empty no-op states, or help/version output.
#  1: Unsupported system, sudo failure, or clear failure.
#  126: A required command exists but is not executable.
#  127: A required command is not found.
#
#  Version History:
#  v1.9 2026-09-08
#       Treat non-actionable or already empty /etc/securetty states as successful no-ops.
#  v1.8 2026-09-06
#       Check uname before using it for system detection.
#  v1.7 2026-07-11
#       Replace the awk {n,} interval expression in usage() with a portable
#       equivalent, since mawk on some systems matches it incorrectly.
#  v1.6 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.5 2025-04-27
#       Add strict error checking for clearing /etc/securetty operation.
#  v1.4 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.3 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.2 2025-03-17
#       Encapsulated all logic in functions and introduced main function.
#  v1.1 2023-11-30
#       Added check to ensure /etc/securetty is a file before clearing it.
#  v1.0 2012-05-21
#       Initial release.
#
########################################################################

# Display full script header information extracted from the top comment block
usage() {
    check_commands awk
    awk '
        BEGIN { in_header = 0 }
        /^#+$/ && length($0) >= 10 { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if the system is Linux
check_system() {
    check_commands uname
    if [ "$(uname -s 2>/dev/null)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Check if required commands are available and executable
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
    check_commands sudo
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Check the type of /etc/securetty and take action
clear_securetty() {
    if [ -f /etc/securetty ]; then
        if [ ! -s /etc/securetty ]; then
            return 0
        fi
        if ! sudo sh -c ": > /etc/securetty"; then
            echo "[ERROR] Failed to clear /etc/securetty." >&2
            exit 1
        fi
        echo "[INFO] Setup completed."
    else
        return 0
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands sh
    check_sudo
    clear_securetty
    return 0
}

# Execute main function
main "$@"
