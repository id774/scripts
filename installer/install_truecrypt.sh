#!/bin/sh

########################################################################
# install_truecrypt.sh: Install and Configure TrueCrypt 7
#
#  Description:
#  This script downloads, installs, and configures TrueCrypt 7 for different
#  platforms (Linux, Windows, macOS, and source builds). It ensures that the
#  necessary dependencies and directories exist and properly sets permissions.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-03-06
#       Updated to be POSIX compliant.
#       Replaced non-POSIX `which` with `command -v`.
#       Added error handling for directory changes.
#       Integrated command existence and sudo privilege checks.
#       Improved logging with `echo` for status updates.
#       Standardized environment setup and permission handling.
#  [Intermediate versions omitted for brevity]
#  v0.1 2010-08-07
#       Stable release.
#
#  Usage:
#  ./install_truecrypt.sh ARCH VERSION [OPTION]
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
    [ -d /usr/local/src/crypt/truecrypt ] || sudo mkdir -p /usr/local/src/crypt/truecrypt
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

# Set proper permissions for TrueCrypt files
set_truecrypt_permission() {
    echo "Setting file permissions..."
    for path in /usr/local/src/crypt/truecrypt /usr/share/truecrypt \
                /usr/local/src/crypt /usr/local/src /usr/bin/truecrypt \
                /usr/bin/truecrypt-uninstall.sh; do
        sudo chown -R "$OWNER" "$path" 2>/dev/null
    done
}

# Save downloaded packages
save_packages() {
    sudo cp "$1" "$2"
    sudo chown "$OWNER" "$2/$1"
}

# Save source files to /usr/local/src/crypt/truecrypt
save_sources() {
    sudo mv * /usr/local/src/crypt/truecrypt
    sudo chown -R "$OWNER" /usr/local/src/crypt/truecrypt
}

# Install TrueCrypt
install_truecrypt() {
    echo "Installing TrueCrypt for architecture: $1, version: $2"
    mkdir install_truecrypt
    cd install_truecrypt || exit 1

    case "$1" in
      linux-i386 | linux-i686)
        FILE_NAME="truecrypt-$2-linux-console-x86.tar.gz"
        ;;
      linux-amd64 | linux-x86_64)
        FILE_NAME="truecrypt-$2-linux-console-x64.tar.gz"
        ;;
      win)
        FILE_NAME="TrueCrypt Setup $2.exe"
        ;;
      mac)
        FILE_NAME="TrueCrypt $2 Mac OS X.dmg"
        ;;
      src)
        FILE_NAME="TrueCrypt $2 Source.tar.gz"
        ;;
    esac

    echo "Downloading $FILE_NAME..."
    wget "http://id774.net/truecrypt/$FILE_NAME"
    [ -n "$3" ] || save_packages "$FILE_NAME" /usr/local/src/crypt/truecrypt

    if [ "$1" = "linux-i386" ] || [ "$1" = "linux-i686" ] || [ "$1" = "linux-amd64" ] || [ "$1" = "linux-x86_64" ]; then
        tar xzvf "$FILE_NAME"
        ./truecrypt-$2-setup-console-$(echo "$1" | awk -F'-' '{print $2}')

        if [ -f "$HOME/.tmp/truecrypt_$2_console_$(echo "$1" | awk -F'-' '{print $2}').tar.gz" ]; then
            cd "$HOME/.tmp" || exit 1
            tar xzvf "truecrypt_$2_console_$(echo "$1" | awk -F'-' '{print $2}').tar.gz"
            rm "truecrypt_$2_console_$(echo "$1" | awk -F'-' '{print $2}').tar.gz"
            [ -d usr ] || exit 1
            sudo cp -Rv usr /
            rm -rf usr
        fi
        file /usr/bin/truecrypt
        set_truecrypt_permission
    else
        save_sources
    fi

    cd ..
    rm -rf install_truecrypt
}

# Main function
main() {
    if [ "$1" = "-h" ]; then
        echo "Usage: ./install_truecrypt.sh ARCH VERSION [OPTION]"
        echo ""
        echo "Options:"
        echo "  -h   Display this help message."
        echo "  -n   Do not save source files after installation."
        exit 0
    fi

    echo "Checking network connectivity..."
    ping -c 1 id774.net >/dev/null 2>&1 || exit 1

    setup_environment
    install_truecrypt "$@"
}

main "$@"
