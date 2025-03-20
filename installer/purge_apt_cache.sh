#!/bin/sh

########################################################################
# purge_apt_cache.sh: Clean Up Removed APT Packages
#
#  Description:
#  This script creates and executes a script to purge residual config
#  files of removed APT packages in Debian-based systems.
#
#  Notes:
#  - This script is intended for Debian-based systems only.
#  - It will permanently remove residual config files of APT packages.
#  - Ensure to review the script before running it.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2025-03-20
#       Added system and command check functions for improved security.
#       Structured script into functions for better modularity and error handling.
#  v1.2 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.1 2023-12-06
#       Added system check, refactored for clarity, and added notes.
#  v1.0 2019-08-29
#       Initial release.
#
#  Usage:
#  Run the script directly without any arguments:
#      ./purge_apt_cache.sh
#
########################################################################

# Function to check if the system is Linux
check_system() {
    if [ "$(uname -s)" != "Linux" ]; then
        echo "Error: This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Check if the system is Debian-based
check_debian() {
    if [ ! -f /etc/debian_version ]; then
        echo "This script only runs on Debian-based systems." >&2
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

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Function to set temporary file location
set_temp_file() {
    SCRIPT_NAME="${TMP:-/tmp}/purge_apt_cache.sh"
}

# Function to generate and execute the cleanup script
perform_cleanup() {
    aptitude search . | grep '^c' | awk '{print $2}' | sed 's/^/sudo apt purge -y /g' > "$SCRIPT_NAME"
    sed -i '1s/^/#!\/bin\/sh\n/' "$SCRIPT_NAME"
    chmod +x "$SCRIPT_NAME"
    echo "The following packages will be purged:"
    cat "$SCRIPT_NAME"
    read -p "Do you want to continue? (y/n): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        "$SCRIPT_NAME"
    fi
    rm "$SCRIPT_NAME"
}

# Main function to execute the Script
main() {
    check_system
    check_commands aptitude awk sed chmod cat rm
    check_debian
    check_sudo
    set_temp_file
    trap 'rm -f "$SCRIPT_NAME"' EXIT
    perform_cleanup
}

# Execute main function
main "$@"
