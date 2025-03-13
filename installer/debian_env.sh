#!/bin/sh

########################################################################
# debian_env.sh: Environment Setup for Debian
#
#  Description:
#  This script configures the base environment for Debian-based systems.
#  It ensures that apt-get is available, performs system updates,
#  and configures essential administrative groups and filesystem tuning.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-03-13
#       Enhanced documentation and comments for better maintainability.
#       Ensured strict POSIX compliance.
#  [Further version history truncated for brevity]
#  v0.1 2011-09-28
#       First version.
#
#  Usage:
#  Run the script directly:
#      ./debian_env.sh
#  Ensure that the required setup scripts exist in the designated directory.
#
#  Notes:
#  - The script is designed for Debian-based systems (Debian, Ubuntu, etc.).
#  - Internet connectivity is required for package updates.
#  - Review and modify configurations as needed before execution.
#
#  Error Conditions:
#  - If apt-get is unavailable, the script exits with an error.
#  - If required commands are missing, execution is halted.
#  - Errors from underlying scripts should be resolved based on their output.
#
########################################################################

# Function to check if the system supports apt-get
check_environment() {
    if ! command -v apt-get >/dev/null 2>&1; then
        echo "Error: apt-get is not available on this system. This script requires a Debian-based environment." >&2
        exit 1
    fi
}

# Function to verify script environment
setup_environment() {
    SCRIPTS="$HOME/scripts"
    if [ ! -d "$SCRIPTS" ]; then
        echo "Error: Directory '$SCRIPTS' does not exist. Please create it or specify the correct path." >&2
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

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Perform system update and upgrade
apt_upgrade() {
    sudo apt-get update &&
    sudo apt-get -y upgrade &&
    sudo apt-get autoclean &&
    sudo apt-get -y autoremove
}

# Create administrative groups
create_admin_group() {
    sudo groupadd -f admin
    sudo groupadd -f wheel
}

# Set up filesystem tuning
setup_tune2fs() {
    "$SCRIPTS/installer/setup_tune2fs.sh"
}

# Main operation function
main() {
    check_environment
    setup_environment
    check_commands sudo vi
    check_sudo

    sudo vi /etc/apt/sources.list
    apt_upgrade
    create_admin_group
    setup_tune2fs
}

# Execute main operations
main "$@"
