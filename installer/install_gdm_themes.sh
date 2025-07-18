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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
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
#  Version History:
#  v2.5 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v2.4 2025-04-28
#       Add error handling to critical operations during GDM themes installation.
#  v2.3 2025-04-22
#       Improved log granularity with [INFO] tags for each processing step.
#  v2.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v2.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v2.0 2025-03-15
#       Unified structure, added system checks, improved error handling.
#  v1.1 2011-09-30
#       Relocation tar file.
#  v1.0 2008-08-15
#       Stable.
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

# Check if the system is Linux
check_system() {
    if [ "$(uname -s)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Check if required commands are available and executable
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

# Check network connectivity
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

# Install GDM themes
install_gdm_themes() {
    echo "[INFO] Checking if /usr/share/gdm/themes exists."
    if [ ! -d /usr/share/gdm/themes ]; then
        echo "[ERROR] /usr/share/gdm/themes does not exist. Aborting." >&2
        exit 1
    fi

    echo "[INFO] Downloading GDM themes archive..."
    if ! wget http://id774.net/archive/gdmthemes.tar.gz; then
        echo "[ERROR] Failed to download gdmthemes.tar.gz." >&2
        exit 1
    fi

    echo "[INFO] Extracting archive to /usr/share/gdm/themes..."
    if ! sudo tar xzvf gdmthemes.tar.gz -C /usr/share/gdm/themes; then
        echo "[ERROR] Failed to extract gdmthemes.tar.gz to /usr/share/gdm/themes." >&2
        exit 1
    fi

    echo "[INFO] Setting ownership to root:root..."
    if ! sudo chown -R root:root /usr/share/gdm/themes; then
        echo "[ERROR] Failed to set ownership for /usr/share/gdm/themes." >&2
        exit 1
    fi

    echo "[INFO] Cleaning up downloaded archive."
    if ! rm -f gdmthemes.tar.gz; then
        echo "[ERROR] Failed to remove gdmthemes.tar.gz." >&2
        exit 1
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands curl wget sudo tar chown rm
    check_network
    check_sudo
    install_gdm_themes

    echo "[INFO] GDM themes installation completed successfully."
    return 0
}

# Execute main function
main "$@"
exit $?
