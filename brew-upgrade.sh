#!/bin/sh

########################################################################
# brew-upgrade.sh: Automated Homebrew Maintenance Script
#
#  Description:
#  This script automates the process of maintaining a Homebrew environment
#  on macOS. It performs the following steps:
#    1. Checks if Homebrew is installed and functional.
#    2. Runs Homebrew diagnostics (`brew doctor`).
#    3. Updates the package list (`brew update`).
#    4. Lists outdated packages (`brew outdated`).
#    5. Upgrades all outdated packages (`brew upgrade`).
#    6. Cleans up old versions and caches (`brew cleanup`).
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.0 2025-01-26
#       Initial release.
#
#  Usage:
#  Run the script directly without any arguments:
#      ./brew-upgrade.sh
#
#  Requirements:
#  - Homebrew must be installed prior to executing this script.
#
########################################################################

# Check if Homebrew is installed
check_environment() {
    if ! command -v brew >/dev/null 2>&1; then
        echo "Error: Homebrew is not installed. Please install Homebrew first." >&2
        exit 1
    fi
}

# Perform Homebrew maintenance tasks
brew_maintenance() {
    echo "Running Homebrew diagnostics..."
    brew doctor

    echo "Updating Homebrew package list..."
    brew update

    echo "Checking for outdated packages..."
    brew outdated

    echo "Upgrading outdated packages..."
    brew upgrade

    echo "Cleaning up old versions and caches..."
    brew cleanup
}

# Main operation
main() {
    check_environment
    brew_maintenance
}

main "$@"
