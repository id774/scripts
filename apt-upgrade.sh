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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run the script directly without any arguments:
#      ./apt-upgrade.sh
#
#  Requirements:
#  - Must be executed with a user that has `sudo` privileges.
#  - Works on Debian-based systems such as Ubuntu.
#
#  Version History:
#  v1.5 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.4 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.3 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.2 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.1 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v1.0 2024-12-09
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

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Check if the system supports apt-get
check_environment() {
    if ! command -v apt-get >/dev/null 2>&1; then
        echo "[ERROR] apt-get is not available on this system. This script requires a Debian-based environment." >&2
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

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac
    check_environment
    check_sudo
    apt_upgrade
    return 0
}

# Execute main function
main "$@"
exit $?
