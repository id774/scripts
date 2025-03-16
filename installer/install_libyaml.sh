#!/bin/sh

########################################################################
# install_libyaml.sh: Installer for libyaml
#
#  Description:
#  This script automates the installation of libyaml by:
#  - Downloading the specified or default version from the official repository.
#  - Compiling and installing the library.
#  - Optionally saving the source files for future use.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v0.2 2025-03-14
#       Added network connection check, Linux system validation, command validation, and improved argument handling.
#  v0.1 2011-01-24
#       First version.
#
#  Usage:
#  Run this script without arguments to install the default version (0.1.4):
#      ./install_libyaml.sh
#  Specify a version to install a different release:
#      ./install_libyaml.sh 0.2.5
#  Skip saving sources by adding a second argument:
#      ./install_libyaml.sh 0.2.5 -n
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
        echo "Error: No network connection detected." >&2
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
    VERSION="${1:-0.1.4}"
}

# Save sources if requested
save_sources() {
    sudo mkdir -p /usr/local/src/libyaml
    sudo cp -av "yaml-$VERSION" /usr/local/src/libyaml/
    sudo chown -R root:root /usr/local/src/libyaml
}

# Install libyaml
install_libyaml() {
    setup_environment "$1"
    mkdir install_libyaml
    cd install_libyaml || exit 1
    wget "http://pyyaml.org/download/libyaml/yaml-$VERSION.tar.gz"
    if [ ! -f "yaml-$VERSION.tar.gz" ]; then
        echo "Error: Failed to download libyaml $VERSION." >&2
        exit 1
    fi
    tar xzvf "yaml-$VERSION.tar.gz"
    cd "yaml-$VERSION" || exit 1
    ./configure --prefix=/usr/local
    make
    sudo make install
    cd ..
    [ -n "$2" ] || save_sources
    cd ..
    rm -rf install_libyaml
}

# Main execution function
main() {
    # Perform initial checks
    check_system
    check_commands curl wget make sudo tar
    check_network
    check_sudo

    # Run the installation process
    install_libyaml "$1" "$2"

    echo "libyaml $VERSION installed successfully."
}

main "$@"
