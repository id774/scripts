#!/bin/sh

########################################################################
# install_cabocha.sh: Installer for CaboCha and CRF++
#
#  Description:
#  This script automates the installation of CaboCha and CRF++ by:
#  - Downloading the default version from the official repository.
#  - Installing required dependencies.
#  - Compiling and installing the application.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-03-20
#       Unified structure, added system checks, improved error handling.
#       Removed arguments, fixed versions, and removed source saving.
#  [Further version history truncated for brevity]
#  v0.1 2012-10-23
#       First version.
#
#  Usage:
#  Run this script without arguments to install the default versions:
#      ./install_cabocha.sh
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - This script is intended for Linux systems only.
#
########################################################################

# Fixed versions
CABOCHA_VERSION="0.67"
CRF_VERSION="0.58"

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
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
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

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Install CRF++
install_crf() {
    wget "http://files.id774.net/archive/CRF%2B%2B-$CRF_VERSION.tar.gz"
    tar xzvf "CRF++-$CRF_VERSION.tar.gz"
    cd "CRF++-$CRF_VERSION" || exit 1
    ./configure
    make
    sudo make install
    cd .. || exit 1
}

# Install CaboCha
install_cabocha() {
    wget "http://files.id774.net/archive/cabocha-$CABOCHA_VERSION.tar.bz2"
    tar xjvf "cabocha-$CABOCHA_VERSION.tar.bz2"
    cd "cabocha-$CABOCHA_VERSION" || exit 1
    ./configure --with-charset=UTF8 --with-posset=IPA --with-mecab-config=$(command -v mecab-config)
    make
    sudo make install
    cd .. || exit 1
}

# Install CRF++ and CaboCha
install_crf_and_cabocha() {
    mkdir install_cabocha || exit 1
    cd install_cabocha || exit 1
    install_crf
    install_cabocha
    cd .. || exit 1
    rm -rf install_cabocha
}

# Main function to execute the script
main() {
    check_system
    check_commands curl wget make sudo apt-get tar
    check_network
    check_sudo
    install_crf_and_cabocha
    echo "CaboCha $CABOCHA_VERSION and CRF++ $CRF_VERSION installed successfully."
}

# Execute main function
main
