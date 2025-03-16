#!/bin/sh

########################################################################
# install_mew.sh: Installer for Mew (Emacs mail client)
#
#  Description:
#  This script automates the installation of Mew by:
#  - Downloading the specified version from the official repository.
#  - Compiling and installing the package for Emacs.
#  - Optionally saving the source files for future use.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v0.3 2025-03-14
#       Added network connection check, Linux system validation, command validation, and improved argument handling.
#  v0.2 2011-12-08
#       The deployment files owned by root.
#  v0.1 2011-03-29
#       First version.
#
#  Usage:
#  Run this script with the Emacs binary path:
#      ./install_mew.sh /usr/bin/emacs
#  Skip saving sources by adding a second argument:
#      ./install_mew.sh /usr/bin/emacs -n
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `wget`, `make`, `sudo`, and `tar` installed.
#  - Must be executed in a shell environment with internet access.
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
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
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

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Setup version and environment
setup_environment() {
    VERSION="6.5"
    case "$OSTYPE" in
        *darwin*) OWNER="root:wheel" ;;
        *) OWNER="root:root" ;;
    esac
}

# Save sources if requested
save_sources() {
    if [ -d /usr/local/src/emacs/mew-$VERSION ]; then
        sudo rm -rf /usr/local/src/emacs/mew-$VERSION
    fi
    if [ ! -d /usr/local/src/emacs ]; then
        sudo mkdir -p /usr/local/src/emacs
    fi
    sudo cp -R "mew-$VERSION" /usr/local/src/emacs/
    sudo chown -R $OWNER /usr/local/src/emacs
}

# Install Mew
install_mew() {
    setup_environment
    mkdir install_mew
    cd install_mew || exit 1
    wget "http://www.mew.org/Release/mew-$VERSION.tar.gz"
    if [ ! -f "mew-$VERSION.tar.gz" ]; then
        echo "Error: Failed to download Mew $VERSION." >&2
        exit 1
    fi
    tar xzvf "mew-$VERSION.tar.gz"
    rm "mew-$VERSION.tar.gz"
    cd "mew-$VERSION" || exit 1
    ./configure --with-elispdir="$HOME/.emacs.d/elisp/3rd-party" \
                --with-etcdir="$HOME/.emacs.d/elisp/3rd-party" \
                --with-emacs="$1"
    make
    make info
    make jinfo
    sudo make install
    sudo make install-jinfo
    cd ..
    [ -n "$2" ] || save_sources
    cd ..
    rm -rf install_mew
}

# Main execution function
main() {
    # Perform initial checks
    check_system
    check_commands curl wget make sudo tar
    check_network
    check_sudo

    # Validate Emacs path
    if [ -z "$1" ]; then
        echo "Error: Emacs binary path must be specified." >&2
        exit 1
    fi

    # Run the installation process
    install_mew "$1" "$2"

    echo "Mew $VERSION installed successfully."
}

main "$@"
