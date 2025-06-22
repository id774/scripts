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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
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

# Function to check if required commands exist
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
        echo "Please set the SCRIPTS variable to the directory containing the script files." >&2
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

# Function to check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges." >&2
        exit 1
    fi
}

# Function to check if required scripts exist
check_required_scripts() {
    for script in "$SCRIPTS/installer/install_brews.sh" "$SCRIPTS/fix_compinit.sh"; do
        if [ ! -f "$script" ]; then
            echo "[ERROR] Required script '$script' not found." >&2
            exit 1
        fi
    done
}

# Reinstall Homebrew
reinstall_homebrew() {
    echo "[INFO] Reinstalling Homebrew..."

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

    echo "[INFO] Homebrew reinstallation completed."
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands curl sudo rm bash
    check_scripts
    check_network
    check_required_scripts
    check_sudo
    reinstall_homebrew
    return 0
}

# Execute main function
main "$@"
exit $?
