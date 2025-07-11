#!/bin/sh

########################################################################
# dpkg-hold.sh: DPKG Package Management Helper Script
#
#  Description:
#  This script, dpkg-hold.sh, facilitates managing package states using the
#  dpkg package management system in Debian-based Linux distributions. It
#  allows users to update the state of a package, display its status, or
#  view usage instructions. The script includes an environment check to
#  ensure it is run on a compatible Debian-based system.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  To set a package state:
#      ./dpkg-hold.sh package-name state
#      (Where 'state' can be 'hold', 'install', etc.)
#
#  To view the current state of a package:
#      ./dpkg-hold.sh package-name
#
#  To display usage information:
#      ./dpkg-hold.sh
#
#  Example:
#      ./dpkg-hold.sh nano install
#      ./dpkg-hold.sh nano
#
#  Version History:
#  v1.9 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.8 2025-04-16
#       Replace inline usage message with usage function call for consistency.
#  v1.7 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.6 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.5 2025-03-16
#       Encapsulated all logic in functions and introduced main function.
#  v1.4 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.3 2024-01-07
#       Updated command existence and execution permission checks
#       using a common function for enhanced reliability and maintainability.
#  v1.2 2023-12-05
#       Added environment check for Debian-based systems and refactored for clarity.
#  v1.1 2011-03-25
#       Modified to operate without requiring sudo privileges.
#  v1.0 2008-08-22
#       Initial release. Provides basic dpkg package management functionality.
#
########################################################################

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
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

# Function to display the current state of a package
show_package_status() {
    dpkg -l "$1"
}

# Function to set the state of a package
set_package_state() {
    echo "$1" "$2" | dpkg --set-selections
    show_package_status "$1"
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_commands dpkg

    if [ -n "$2" ]; then
        # If two arguments are provided, set the package state
        set_package_state "$1" "$2"
    elif [ -n "$1" ]; then
        # If only one argument is provided, display the package status
        show_package_status "$1"
    else
        # If no arguments are provided, display usage instructions
        usage
    fi
    return 0
}

# Execute main function
main "$@"
exit $?
