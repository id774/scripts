#!/bin/sh

########################################################################
# purge_kernels.sh: Purge Old Kernels in Ubuntu
#
#  Description:
#  This script removes old kernels from an Ubuntu or Ubuntu-based system,
#  keeping the currently running kernel. It ensures system compatibility
#  and prevents accidental removal of the active kernel.
#
#  The script performs the following operations:
#  - Checks if the system is Ubuntu-based before execution.
#  - Identifies and lists installed kernel packages.
#  - Excludes the currently running kernel from the removal list.
#  - Safely removes only outdated kernels while preserving system stability.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run the script directly without any arguments:
#      ./purge_kernels.sh
#
#  Notes:
#  - This script is intended for Ubuntu-based systems only.
#  - The currently running kernel will always be preserved.
#  - If no old kernels are found, the script exits without making changes.
#  - Ensure to review the listed kernels before executing the script.
#
#  Version History:
#  v1.6 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.5 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.4 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.3 2025-03-20
#       Added system and command check functions for improved security.
#       Structured script into functions for better modularity and error handling.
#  v1.2 2025-02-04
#       Switched from aptitude to apt for broader compatibility.
#       Improved kernel identification using dpkg --list.
#       Added a check to ensure old kernels exist before attempting removal.
#  v1.1 2023-12-06
#       Refactored for improved readability, naming, and system checking.
#  v1.0 2013-11-29
#       Initial release.
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

# Check if the system is Ubuntu-based
check_ubuntu() {
    if [ ! -f /etc/lsb-release ] || [ ! -f /etc/debian_version ]; then
        echo "[ERROR] This script only runs on Ubuntu or Ubuntu-based systems." >&2
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

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Get the currently running kernel version
get_current_kernel() {
    CURKERNEL=$(uname -r | sed 's/-*[a-z]//g' | sed 's/-386//g')
}

# Find and list old kernels
list_old_kernels() {
    OLDKERNELS=$(dpkg --list | awk '/^ii/ && /linux-image-[0-9]/ {print $2}' | grep -v "$CURKERNEL")
    if [ -z "$OLDKERNELS" ]; then
        echo "[INFO] No old kernels to remove."
        exit 0
    fi
}

# Remove old kernels safely
remove_old_kernels() {
    echo "[INFO] Purging old kernels: $OLDKERNELS"
    sudo apt purge -y $OLDKERNELS
    echo "[INFO] Kernel cleanup completed."
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_ubuntu
    check_commands apt dpkg awk grep sed sudo uname
    check_sudo
    get_current_kernel
    list_old_kernels
    remove_old_kernels
    return 0
}

# Execute main function
main "$@"
exit $?
