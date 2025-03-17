#!/bin/sh

########################################################################
# install_gdm_themes.sh: Installer for GDM Themes
#
#  Description:
#  This script automates the installation of GDM themes by:
#  - Downloading the theme archive from the official repository.
#  - Extracting and installing the themes in the correct directory.
#  - Setting appropriate ownership and permissions.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.0 2025-03-15
#       Unified structure, added system checks, improved error handling.
#  v1.1 2011-09-30
#       Relocation tar file.
#  v1.0 2008-08-15
#       Stable.
#
#  Usage:
#  Run this script without arguments to install GDM themes:
#      ./install_gdm_themes.sh
#
#  Requirements:
#  - The user must have `wget`, `sudo`, and `tar` installed.
#  - Network connectivity is required to download the theme archive.
#  - This script is intended for Linux systems only.
#
########################################################################

# Function to check if the system is Linux
check_system() {
    if [ "$(uname -s)" != "Linux" ]; then
        echo "Error: This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Function to check network connectivity
check_network() {
    if ! curl -s --head --connect-timeout 5 http://clients3.google.com/generate_204 >/dev/null; then
        echo "Error: No network connection detected. Please check your internet access." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Install GDM themes
install_gdm_themes() {
    [ -d /usr/share/gdm/themes ] || exit 1
    wget http://id774.net/archive/gdmthemes.tar.gz
    sudo tar xzvf gdmthemes.tar.gz -C /usr/share/gdm/themes
    sudo chown -R root:root /usr/share/gdm/themes
    rm gdmthemes.tar.gz
}

# Main function to execute the script
main() {
    check_system
    check_commands curl wget sudo tar
    check_network
    check_sudo
    install_gdm_themes
    echo "GDM themes installation completed successfully."
}

# Execute main function
main "$@"
