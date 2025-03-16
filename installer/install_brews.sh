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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-03-16
#       Added macOS system check and network connectivity validation.
#  v1.0 2025-01-16
#       Initial stable release. Added support for installing essential tools like
#       OpenSSL, Coreutils, Vim, and MeCab via Homebrew.
#  v0.1 2016-04-18
#       First release.
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
########################################################################

# Function to check if the system is macOS
check_system() {
    if [ "$(uname)" != "Darwin" ]; then
        echo "Error: This script is intended for macOS only." >&2
        exit 1
    fi
}

# Function to check network connectivity
check_network() {
    if ! curl -s --head --connect-timeout 5 http://clients3.google.com/generate_204 >/dev/null; then
        echo "Error: No network connection detected. Please check your internet access." >&2
        exit 1
    fi
}

# Function to check if Homebrew is installed
check_homebrew() {
    if ! command -v brew >/dev/null 2>&1; then
        echo "Error: Homebrew is not installed. Please install Homebrew first." >&2
        exit 1
    fi
}

# Main execution function
main() {
    check_system
    check_network
    check_homebrew

    # Check Homebrew environment
    echo "Running 'brew doctor' to check the system's Homebrew environment..."
    brew doctor

    # Update Homebrew
    echo "Updating Homebrew packages..."
    brew update

    # Install essential tools and libraries
    echo "Installing essential tools and libraries using Homebrew..."
    brew install openssl
    brew link openssl --force
    brew install wget
    brew install nkf
    brew install vim
    brew install freetype
    brew install mecab
    brew install cabocha
    brew install ta-lib
    brew install trash
    brew install coreutils
    brew install findutils
    brew install moreutils
    brew install binutils

    echo "All specified packages have been successfully installed."
}

main "$@"
