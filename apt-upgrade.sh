#!/bin/sh

########################################################################
# apt-upgrade.sh: Automated System Update and Upgrade Script
#
#  Description:
#  This script automates the process of updating and upgrading a Debian-based
#  system using `apt-get`. It performs the following steps:
#    1. Checks if the environment supports `apt-get`.
#    2. Updates the package list (`apt-get update`).
#    3. Upgrades all installed packages to the latest versions (`apt-get upgrade`).
#    4. Cleans up unnecessary cached packages (`apt-get autoclean`).
#    5. Removes unused packages and dependencies (`apt-get autoremove`).
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2025-03-13
#       Improved error handling.
#  v1.1 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v1.0 2024-12-09
#       Initial release.
#
#  Usage:
#  Run the script directly without any arguments:
#      ./apt-upgrade.sh
#
#  Requirements:
#  - Must be executed with a user that has `sudo` privileges.
#  - Works on Debian-based systems such as Ubuntu.
#
########################################################################

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Check if the system supports apt-get
check_environment() {
    if ! command -v apt-get >/dev/null 2>&1; then
        echo "Error: apt-get is not available on this system. This script requires a Debian-based environment." >&2
        exit 1
    fi
}

# System update and upgrade
apt_upgrade() {
    sudo apt-get update &&
    sudo apt-get -y upgrade &&
    sudo apt-get autoclean &&
    sudo apt-get -y autoremove
}

# Main operation
main() {
    check_environment
    check_sudo
    apt_upgrade
}

main "$@"
