#!/bin/sh

########################################################################
# install_gdm_themes2.sh: Installer for GDM Themes 2
#
#  Description:
#  This script automates the installation of GDM Themes 2 by:
#  - Downloading the theme archive from the official repository.
#  - Extracting and installing the themes in the correct directory.
#  - Setting appropriate ownership and permissions.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v2.0 2025-03-15
#       Unified structure, added system checks, improved error handling.
#  v1.1 2011-09-30
#       Relocation tar file.
#  v1.0 2008-11-17
#       Stable.
#
#  Usage:
#  Run this script without arguments to install GDM Themes 2:
#      ./install_gdm_themes2.sh
#
#  Requirements:
#  - The user must have `wget`, `sudo`, and `tar` installed.
#  - Network connectivity is required to download the theme archive.
#  - This script is intended for Linux systems only.
#
########################################################################

# Display script usage information
usage() {
    awk '
        BEGIN { in_usage = 0 }
        /^#  Usage:/ { in_usage = 1; print substr($0, 4); next }
        /^#{10}/ { if (in_usage) exit }
        in_usage && /^#/ { print substr($0, 4) }
    ' "$0"
    exit 0
}

# Function to check if the system is Linux
check_system() {
    if [ "$(uname -s)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Function to check required commands
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

# Function to check network connectivity
check_network() {
    if ! curl -s --head --connect-timeout 5 http://clients3.google.com/generate_204 >/dev/null; then
        echo "[ERROR] No network connection detected. Please check your internet access." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Install GDM Themes 2
install_gdm_themes2() {
    [ -d /usr/share/gdm/themes ] || exit 1
    wget http://id774.net/archive/gdmthemes2.tar.gz
    sudo tar xzvf gdmthemes2.tar.gz -C /usr/share/gdm/themes
    sudo chown -R root:root /usr/share/gdm/themes
    rm gdmthemes2.tar.gz
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    check_commands curl wget sudo tar chown rm
    check_network
    check_sudo
    install_gdm_themes2
    echo "GDM Themes 2 installation completed successfully."
}

# Execute main function
main "$@"
