#!/bin/sh

########################################################################
# setup_aptconf.sh: Configure apt.conf for APT
#
#  Description:
#  This script deploys a custom `apt.conf` file and allows manual editing.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-15
#       Added system, command, and script checks.
#       Improved error handling and user prompts.
#       Ensured idempotent execution.
#  v0.1 2008-11-06
#       Initial version.
#
#  Usage:
#  Run this script to deploy and edit the APT configuration file:
#      ./setup_aptconf.sh
#
#  Requirements:
#  - Must be executed with sudo privileges.
#  - The `$SCRIPTS` environment variable must be set.
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

# Function to check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the directory containing the apt.conf file." >&2
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

# Function to deploy the apt.conf file
deploy_aptconf() {
    echo "[INFO] Deploying apt.conf..."
    sudo cp -v "$SCRIPTS/etc/apt.conf" /etc/apt/apt.conf
    sudo chmod 644 /etc/apt/apt.conf
    sudo chown root:root /etc/apt/apt.conf
}

# Function to allow manual editing of apt.conf
edit_aptconf() {
    echo "[INFO] Opening apt.conf for manual editing..."
    echo "Please edit /etc/apt/apt.conf"
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    check_commands sudo cp chmod chown
    check_scripts
    check_sudo

    deploy_aptconf
    edit_aptconf

    echo "[INFO] APT configuration setup completed successfully."
    return 0
}

# Execute main function
main "$@"
exit $?
