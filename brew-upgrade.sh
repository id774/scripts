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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run the script directly without any arguments:
#      ./brew-upgrade.sh
#
#  Requirements:
#  - Homebrew must be installed prior to executing this script.
#
#  Version History:
#  v1.4 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.3 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.2 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.1 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.0 2025-01-26
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

# Check if Homebrew is installed
check_environment() {
    if ! command -v brew >/dev/null 2>&1; then
        echo "[ERROR] Homebrew is not installed. Please install Homebrew first." >&2
        exit 1
    fi
}

# Perform Homebrew maintenance tasks
brew_maintenance() {
    echo "[INFO] Running Homebrew diagnostics..."
    brew doctor

    echo "[INFO] Updating Homebrew package list..."
    brew update

    echo "[INFO] Checking for outdated packages..."
    brew outdated

    echo "[INFO] Upgrading outdated packages..."
    brew upgrade

    echo "[INFO] Cleaning up old versions and caches..."
    brew cleanup
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac
    check_environment
    brew_maintenance
    return 0
}

# Execute main function
main "$@"
