#!/bin/sh

########################################################################
# install_veracrypt.sh: Install and Configure VeraCrypt
#
#  Description:
#  This script downloads, installs, and configures VeraCrypt for Linux.
#  It ensures that the necessary dependencies and directories exist and
#  properly sets permissions. The script automatically determines the system
#  architecture and installs the predefined version (1.25.9).
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.0 2025-03-07
#       Updated to be POSIX compliant.
#       Replaced non-POSIX `which` with `command -v`.
#       Added error handling for directory changes.
#       Integrated command existence and sudo privilege checks.
#       Improved logging with `echo` for status updates.
#       Standardized environment setup and permission handling.
#       Automatically detect system architecture.
#       Fixed VeraCrypt version to 1.25.9 (no longer user-specified).
#       Simplified script by removing version argument.
#  v0.1 2023-01-18
#       Forked from TrueCrypt Installer.
#
#  Usage:
#  ./install_veracrypt.sh [OPTION]
#
#  Options:
#  -h   Display this help message.
#  -n   Do not save source files after installation.
#
########################################################################

# Function to check required commands
check_commands() {
    for cmd in wget tar sudo rm mkdir cp chown ping file uname; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Determine system architecture
get_architecture() {
    case $(uname -m) in
        i386|i686)
            ARCH="x86"
            ;;
        x86_64|amd64)
            ARCH="x64"
            ;;
        arm*|aarch64)
            echo "Error: VeraCrypt does not support ARM architectures." >&2
            exit 1
            ;;
        *)
            echo "Error: Unsupported architecture." >&2
            exit 1
            ;;
    esac
}

# Configure environment settings (Linux only)
setup_environment() {
    echo "Setting up environment..."
    check_commands wget tar sudo rm mkdir cp chown ping file uname dmsetup

    # Ensure the script is running on Linux
    if [ "$(uname)" != "Linux" ]; then
        echo "Error: This script can only be run on Linux." >&2
        exit 1
    fi

    echo "Checking network connectivity..."
    ping -c 1 id774.net >/dev/null 2>&1 || exit 1

    check_sudo

    [ -d /usr/local/src/crypt/veracrypt ] || sudo mkdir -p /usr/local/src/crypt/veracrypt
    [ -d "$HOME/.tmp" ] || mkdir "$HOME/.tmp"

    # Ensure TMP is correctly set to $HOME/.tmp
    if [ "${TMP:-}" != "$HOME/.tmp" ]; then
        echo "Error: TMP environment variable is not set correctly. Expected '$HOME/.tmp', but got '${TMP:-unset}'."
        exit 1
    fi

    OWNER=root:root
}

# Save downloaded packages
save_packages() {
    sudo cp "$1" "$2"
    sudo chown "$OWNER" "$2/$1"
}

# Set proper permissions for VeraCrypt files
set_veracrypt_permission() {
    echo "Setting file permissions..."
    for path in /usr/local/src/crypt/veracrypt /usr/share/veracrypt \
                /usr/share/doc/veracrypt /usr/sbin/mount.veracrypt \
                /usr/local/src/crypt /usr/local/src /usr/bin/veracrypt \
                /usr/bin/veracrypt-uninstall.sh; do
        sudo chown -R "$OWNER" "$path" 2>/dev/null
    done
}

# Install VeraCrypt
install_veracrypt() {
    setup_environment

    get_architecture
    echo "Installing VeraCrypt version 1.25.9 for architecture: $ARCH"
    mkdir install_veracrypt
    cd install_veracrypt || exit 1

    FILE_NAME="veracrypt-1.25.9-setup-console-$ARCH"
    echo "Downloading $FILE_NAME..."
    wget "http://id774.net/veracrypt/$FILE_NAME"
    [ "$1" != "-n" ] && save_packages "$FILE_NAME" /usr/local/src/crypt/veracrypt

    chmod +x "./$FILE_NAME" && "./$FILE_NAME"
    file /usr/bin/veracrypt
    set_veracrypt_permission

    cd ..
    rm -rf install_veracrypt
}

# Main function
main() {
    if [ "$1" = "-h" ]; then
        echo "Usage: ./install_veracrypt.sh [OPTION]"
        echo ""
        echo "Options:"
        echo "  -h   Display this help message."
        echo "  -n   Do not save source files after installation."
        exit 0
    fi

    install_veracrypt "$@"
}

main "$@"
