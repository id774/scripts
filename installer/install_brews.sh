#!/bin/sh

########################################################################
# install_brews.sh: Bulk Homebrew Library Install Script
#
#  Description:
#  This script installs a set of essential tools and libraries on macOS
#  using Homebrew. It ensures that required software is installed and
#  configured for development, text processing, system administration,
#  and other tasks.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script in a terminal to set up your macOS environment.
#  Examples:
#     ./install_brews.sh
#
#  Requirements:
#  - Homebrew must be installed prior to executing this script.
#
#  Exit Codes:
#  0: Success - All packages were installed successfully.
#  1: Error - Homebrew is not installed or a critical issue occurred.
#
#  Notes:
#  - This script ensures the use of GNU Coreutils on macOS for consistent
#    behavior across platforms.
#  - Force-links OpenSSL to ensure compatibility with applications requiring
#    the latest version.
#  - `trash` is installed for safer file deletions, replacing `rm`.
#
#  Version History:
#  v1.5 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.4 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.3 2025-03-27
#       Added explicit 'brew cleanup' step to remove old versions and caches.
#  v1.2 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.1 2025-03-16
#       Added macOS system check and network connectivity validation.
#  v1.0 2025-01-16
#       Initial stable release. Added support for installing essential tools like
#       OpenSSL, Coreutils, Vim, and MeCab via Homebrew.
#  v0.1 2016-04-18
#       First release.
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

# Function to check if the system is macOS
check_system() {
    if [ "$(uname)" != "Darwin" ]; then
        echo "[ERROR] This script is intended for macOS only." >&2
        exit 1
    fi
}

# Function to check network connectivity
check_network() {
    if ! curl -s --head --connect-timeout 5 http://clients3.google.com/generate_204 >/dev/null; then
        echo "[ERROR] No network connection detected. Please check your internet access." >&2
        exit 1
    fi
}

# Function to check if Homebrew is installed
check_homebrew() {
    if ! command -v brew >/dev/null 2>&1; then
        echo "[ERROR] Homebrew is not installed. Please install Homebrew first." >&2
        exit 1
    fi
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_network
    check_homebrew

    # Check Homebrew environment
    echo "[INFO] Running 'brew doctor' to check the system's Homebrew environment..."
    brew doctor

    # Update Homebrew
    echo "[INFO] Updating Homebrew packages..."
    brew update

    # Install essential tools and libraries
    echo "[INFO] Installing essential tools and libraries using Homebrew..."
    brew install openssl
    brew link openssl --force
    brew install wget
    brew install nkf
    brew install vim
    brew install nvim
    brew install freetype
    brew install rsync
    brew install smartmontools
    brew install mecab
    brew install cabocha
    brew install ta-lib
    brew install trash
    brew install coreutils
    brew install findutils
    brew install moreutils
    brew install binutils

    # Cleanup old versions and caches to free up disk space
    echo "[INFO] Cleaning up old versions and caches..."
    brew cleanup

    echo "[INFO] All specified brew packages have been installed."
    return 0
}

# Execute main function
main "$@"
exit $?
