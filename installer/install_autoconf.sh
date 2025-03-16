#!/bin/sh

########################################################################
# install_autoconf.sh: Installer for Autoconf
#
#  Description:
#  This script automates the installation of Autoconf by:
#  - Downloading the specified or default version from the GNU FTP server.
#  - Compiling and installing it to the system.
#  - Optionally saving the source files for future use.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v0.2 2025-03-14
#       Added network connection check and improved argument handling.
#  v0.1 2011-04-26
#       First version.
#
#  Usage:
#  Run this script without arguments to install the default version (2.69):
#      ./install_autoconf.sh
#  Specify a version to install a different release:
#      ./install_autoconf.sh 2.71
#  Skip saving sources by adding a second argument:
#      ./install_autoconf.sh 2.71 -n
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `wget`, `make`, and `sudo` installed.
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

# Setup version environment
setup_environment() {
    VERSION="${1:-2.69}"
}

# Save sources if requested
save_sources() {
    sudo mkdir -p /usr/local/src/autoconf
    sudo cp -av autoconf-$VERSION /usr/local/src/autoconf/
    sudo chown -R root:root /usr/local/src/autoconf
}

# Install Autoconf
install_autoconf() {
    setup_environment "$1"
    mkdir install_autoconf
    cd install_autoconf || exit 1
    wget "ftp://ftp.gnu.org/gnu/autoconf/autoconf-$VERSION.tar.gz"
    tar xzvf "autoconf-$VERSION.tar.gz"
    cd "autoconf-$VERSION" || exit 1
    ./configure --prefix=/usr
    make
    sudo make install
    cd ..
    [ -n "$2" ] || save_sources
    cd ..
    rm -rf install_autoconf
}

# Main execution function
main() {
    # Perform initial checks
    check_system
    check_commands curl wget make sudo tar
    check_network
    check_sudo

    # Run the installation process
    install_autoconf "$1" "$2"

    echo "Autoconf $VERSION installed successfully."
}

main "$@"
