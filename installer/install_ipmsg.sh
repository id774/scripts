#!/bin/sh

########################################################################
# install_ipmsg.sh: Installer for IP Messenger for Linux
#
#  Description:
#  This script automates the installation of IP Messenger by:
#  - Downloading the specified or default version from the official repository.
#  - Installing required dependencies.
#  - Compiling and installing the application.
#  - Optionally saving the source files for future use.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.4 2025-03-14
#       Added network connection check, Linux system validation, command validation, and improved argument handling.
#  v1.3 2010-09-16
#       Refactoring.
#  v1.2 2010-03-07
#       Refactoring and update to 0.9.6.
#  v1.1 2008-12-15
#       Set permission of source.
#  v1.0 2008-08-15
#       Stable.
#
#  Usage:
#  Run this script without arguments to install the default version (0.9.6):
#      ./install_ipmsg.sh
#  Specify a version to install a different release:
#      ./install_ipmsg.sh 1.0.1
#  Skip saving sources by adding a second argument:
#      ./install_ipmsg.sh 1.0.1 -n
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `wget`, `make`, `sudo`, and `apt-get` installed.
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
    if ! ping -c 1 id774.net >/dev/null 2>&1; then
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
    VERSION="${1:-0.9.6}"
    IPMSG="g2ipmsg-$VERSION"
}

# Save sources if requested
save_sources() {
    sudo mkdir -p /usr/local/src/ipmsg
    sudo cp -av "$IPMSG" /usr/local/src/ipmsg/
    sudo chown -R root:root /usr/local/src/ipmsg
}

# Install IP Messenger
install_ipmsg() {
    setup_environment "$1"
    sudo apt-get -y install libgtk2.0-dev libgnomeui-dev libpanelappletmm-2.6-dev
    mkdir install_ipmsg
    cd install_ipmsg || exit 1
    wget "http://www.ipmsg.org/archive/$IPMSG.tar.gz"
    if [ ! -f "$IPMSG.tar.gz" ]; then
        echo "Error: Failed to download IP Messenger $VERSION." >&2
        exit 1
    fi
    tar xzvf "$IPMSG.tar.gz"
    cd "$IPMSG" || exit 1
    ./configure
    make
    sudo make install
    cd ..
    [ -n "$2" ] || save_sources
    cd ..
    rm -rf install_ipmsg
}

# Main execution function
main() {
    # Perform initial checks
    check_system
    check_commands wget make sudo apt-get tar ping
    check_network
    check_sudo

    # Run the installation process
    install_ipmsg "$1" "$2"

    echo "IP Messenger $VERSION installed successfully."
}

main "$@"
