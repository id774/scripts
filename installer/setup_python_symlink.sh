#!/bin/sh

########################################################################
# setup_python_symlink.sh: Install python-is-python3 if needed
#
#  Description:
#  This script ensures compatibility with legacy scripts that invoke
#  'python' by installing the 'python-is-python3' package when
#  '/usr/bin/python' is not present on the system.
#
#  An uninstall option is also provided:
#  - Removes the package 'python-is-python3' if it is installed.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  To install:
#      ./setup_python_symlink.sh
#
#  To uninstall:
#      ./setup_python_symlink.sh --uninstall
#
#  Requirements:
#  - Must be run with root privileges.
#  - The 'apt' command must be available on the system.
#
#  Version History:
#  v1.0 2025-07-16
#       Initial version.
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

# Check if the system is Linux
check_system() {
    if [ "$(uname -s 2>/dev/null)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
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

# Install python-is-python3 only if /usr/bin/python does not exist
install_python_symlink() {
    if [ ! -x /usr/bin/python ]; then
        echo "[INFO] /usr/bin/python not found. Installing python-is-python3..."
        sudo apt-get update && sudo apt-get install -y python-is-python3 || {
            echo "[ERROR] Failed to install python-is-python3" >&2
            exit 1
        }
        echo "[INFO] Successfully installed python-is-python3"
    else
        echo "[INFO] /usr/bin/python already exists. No action taken."
    fi
}

# Uninstall python-is-python3 if installed
uninstall_python_symlink() {
    if dpkg -l | grep -q '^ii\s\+python-is-python3\s'; then
        echo "[INFO] Removing python-is-python3..."
        sudo apt-get remove -y python-is-python3 || {
            echo "[ERROR] Failed to remove python-is-python3" >&2
            exit 1
        }
        echo "[INFO] Uninstallation completed."
    else
        echo "[INFO] python-is-python3 is not installed. Nothing to remove."
    fi
}

# Check functions
check_environment() {
    check_system
    check_sudo
    check_commands "$@"
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
        -u|--uninstall)
            check_environment apt-get dpkg
            uninstall_python_symlink
            ;;
        *)
            check_environment apt-get
            install_python_symlink
            ;;
    esac
    return 0
}

# Execute main function
main "$@"
exit $?
