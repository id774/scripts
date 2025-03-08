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
#  v1.0 2025-03-07
#       Updated to be POSIX compliant.
#       Replaced non-POSIX `which` with `command -v`.
#       Added error handling for directory changes.
#       Integrated command existence and sudo privilege checks.
#       Improved logging with `echo` for status updates.
#       Standardized environment setup and permission handling.
#       Automatically detect system architecture.
#  [Intermediate versions omitted for brevity]
#  v0.1 2010-08-07
#       Stable release.
#
#  Usage:
#  ./install_truecrypt.sh [VERSION] [OPTION]
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
            echo "Error: TrueCrypt does not support ARM architectures."
            exit 1
            ;;
        *)
            echo "Error: Unsupported architecture."
            exit 1
            ;;
    esac
}

# Validate version input
validate_version() {
    case "$1" in
        "7.0a"|"7.1"|"7.1a")
            VERSION="$1"
            ;;
        "")
            echo "No version specified. Defaulting to 7.1a."
            VERSION="7.1a"
            ;;
        *)
            echo "Error: Unsupported TrueCrypt version specified. Allowed versions: 7.0a, 7.1, 7.1a."
            exit 1
            ;;
    esac
}

# Configure environment settings based on OS type
setup_environment() {
    echo "Setting up environment..."
    check_commands wget tar sudo rm mkdir cp chown ping file uname dmsetup

    # Ensure the script is running on Linux
    if [ "$(uname)" != "Linux" ]; then
        echo "Error: This script can only be run on Linux."
        exit 1
    fi

    check_sudo

    [ -d /usr/local/src/crypt/truecrypt ] || sudo mkdir -p /usr/local/src/crypt/truecrypt
    [ -d "$HOME/.tmp" ] || mkdir "$HOME/.tmp"

    OWNER=root:root
}

# Save downloaded packages
save_packages() {
    sudo cp "$1" "$2"
    sudo chown "$OWNER" "$2/$1"
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

# Install TrueCrypt
install_truecrypt() {
    get_architecture
    validate_version "$1"
    echo "Installing TrueCrypt version: $VERSION for architecture: $ARCH"
    mkdir install_truecrypt
    cd install_truecrypt || exit 1

    TAR_NAME="truecrypt-$VERSION-linux-console-$ARCH.tar.gz"
    EXEC_NAME="truecrypt-$VERSION-setup-console-$ARCH"
    echo "Downloading $TAR_NAME..."
    wget "http://id774.net/truecrypt/$TAR_NAME"
    tar xzvf "$TAR_NAME"
    [ -n "$2" ] || save_packages "$TAR_NAME" /usr/local/src/crypt/truecrypt

    chmod +x "./$EXEC_NAME" && "./$EXEC_NAME"
    file /usr/bin/truecrypt
    set_truecrypt_permission

    cd ..
    rm -rf install_truecrypt
}

# Main function
main() {
    if [ "$1" = "-h" ]; then
        echo "Usage: ./install_truecrypt.sh [VERSION] [OPTION]"
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
