#!/bin/sh

########################################################################
# install_veracrypt.sh: Install and Configure VeraCrypt
#
#  Description:
#  This script downloads, installs, and configures VeraCrypt for different
#  platforms (Linux, Windows, macOS, and source builds). It ensures that the
#  necessary dependencies and directories exist and properly sets permissions.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-03-07
#       Updated to be POSIX compliant.
#       Replaced non-POSIX `which` with `command -v`.
#       Added error handling for directory changes.
#       Integrated command existence and sudo privilege checks.
#       Improved logging with `echo` for status updates.
#       Standardized environment setup and permission handling.
#  v0.1 2023-01-18
#       Forked from TrueCrypt Installer.
#
#  Usage:
#  ./install_veracrypt.sh ARCH VERSION [OPTION]
#
#  Options:
#  -h   Display this help message.
#  -n   Do not save source files after installation.
#
########################################################################

# Function to check required commands
check_commands() {
    for cmd in wget tar sudo rm mkdir cp chown ping file; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again."
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions."
            exit 126
        fi
    done
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access."
        exit 1
    fi
}

# Configure environment settings based on OS type
setup_environment() {
    echo "Setting up environment..."
    check_commands wget tar sudo rm mkdir cp chown ping file
    check_sudo

    command -v dmsetup >/dev/null 2>&1 || sudo apt-get -y install dmsetup
    [ -d /usr/local/src/crypt/veracrypt ] || sudo mkdir -p /usr/local/src/crypt/veracrypt
    [ -d "$HOME/.tmp" ] || mkdir "$HOME/.tmp"

    case $(uname) in
      Darwin)
        OWNER=root:wheel
        ;;
      *)
        OWNER=root:root
        ;;
    esac
}

# Set proper permissions for VeraCrypt files
set_veracrypt_permission() {
    echo "Setting file permissions..."
    for path in /usr/local/src/crypt/veracrypt /usr/share/veracrypt \
                /usr/local/src/crypt /usr/local/src /usr/bin/veracrypt \
                /usr/bin/veracrypt-uninstall.sh; do
        sudo chown -R "$OWNER" "$path" 2>/dev/null
    done
}

# Save downloaded packages
save_packages() {
    sudo cp "$1" "$2"
    sudo chown "$OWNER" "$2/$1"
}

# Save source files to /usr/local/src/crypt/veracrypt
save_sources() {
    sudo mv * /usr/local/src/crypt/veracrypt
    sudo chown -R "$OWNER" /usr/local/src/crypt/veracrypt
}

# Install VeraCrypt
install_veracrypt() {
    echo "Installing VeraCrypt for architecture: $1, version: $2"
    mkdir install_veracrypt
    cd install_veracrypt || exit 1

    FILE_NAME="veracrypt-$2-setup-console-$(echo "$1" | awk -F'-' '{print $2}')"
    echo "Downloading $FILE_NAME..."
    wget "http://id774.net/veracrypt/$FILE_NAME"
    [ -n "$3" ] || save_packages "$FILE_NAME" /usr/local/src/crypt/veracrypt

    chmod +x "./$FILE_NAME" && "./$FILE_NAME"
    file /usr/bin/veracrypt
    set_veracrypt_permission

    cd ..
    rm -rf install_veracrypt
}

# Main function
main() {
    if [ "$1" = "-h" ]; then
        echo "Usage: ./install_veracrypt.sh ARCH VERSION [OPTION]"
        echo ""
        echo "Options:"
        echo "  -h   Display this help message."
        echo "  -n   Do not save source files after installation."
        exit 0
    fi

    echo "Checking network connectivity..."
    ping -c 1 id774.net >/dev/null 2>&1 || exit 1

    setup_environment
    install_veracrypt "$@"
}

main "$@"
