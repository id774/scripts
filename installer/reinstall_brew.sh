#!/bin/sh

########################################################################
# reinstall_brew.sh: Reinstall Homebrew on macOS
#
#  Description:
#  This script completely removes and reinstalls Homebrew on macOS by:
#  - Adjusting ownership of relevant directories.
#  - Uninstalling Homebrew non-interactively.
#  - Removing residual Homebrew directories.
#  - Installing Homebrew from the official source.
#  - Running a batch installation script for required packages.
#  - Fixing compinit issues after installation.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-03-16
#       Initial stable release with full reinstall process.
#  v0.1 2022-09-22
#       First version.
#
#  Usage:
#  Run this script to reinstall Homebrew:
#      ./reinstall_brew.sh
#
#  Requirements:
#  - Must be executed on macOS.
#  - Requires `sudo` privileges for modifying system directories.
#
########################################################################

# Function to check if the system is macOS
check_system() {
    if [ "$(uname)" != "Darwin" ]; then
        echo "Error: This script is intended for macOS only." >&2
        exit 1
    fi
}

# Function to check if required commands exist
check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install it and try again." >&2
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Function to check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "Error: SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the directory containing the script files." >&2
        exit 1
    fi
}

# Function to check network connectivity
check_network() {
    if ! ping -c 1 id774.net >/dev/null 2>&1; then
        echo "Error: No network connection detected. Please check your internet access." >&2
        exit 1
    fi
}

# Function to check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges." >&2
        exit 1
    fi
}

# Function to check if required scripts exist
check_required_scripts() {
    for script in "$SCRIPTS/installer/install_brews.sh" "$SCRIPTS/fix_compinit.sh"; do
        if [ ! -f "$script" ]; then
            echo "Error: Required script '$script' not found." >&2
            exit 1
        fi
    done
}

# Reinstall Homebrew
reinstall_homebrew() {
    echo "Reinstalling Homebrew..."

    # Uninstall Homebrew non-interactively
    NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/uninstall.sh)"

    # Remove residual directories
    sudo rm -rf /usr/local/Cellar /usr/local/Homebrew

    # Install Homebrew
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

    # Run batch installation script for required packages
    "$SCRIPTS/installer/install_brews.sh"

    # Fix compinit issues
    "$SCRIPTS/fix_compinit.sh"

    echo "Homebrew reinstallation completed successfully."
}

# Main execution function
main() {
    check_system
    check_commands curl sudo rm bash
    check_scripts
    check_network
    check_required_scripts
    check_sudo
    reinstall_homebrew
}

main "$@"
