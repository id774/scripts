#!/bin/sh

########################################################################
# install_cabocha.sh: Installer for CaboCha and CRF++
#
#  Description:
#  This script automates the installation of CaboCha and CRF++ by:
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
#  v1.0 2025-03-16
#       Unified structure, added system checks, improved error handling.
#       Removed install_binding, unified structure, improved maintainability.
#  v0.7 2015-06-01
#       Specify mecab-config.
#  v0.6 2015-05-29
#       Change URL, fix args bug.
#  v0.5 2014-02-10
#       Refactoring.
#  v0.4 2014-01-27
#       Fix erase process.
#  v0.3 2014-01-19
#       Not use https.
#  v0.2 2013-10-29
#       Update download URL.
#  v0.1 2012-10-23
#       First.
#
#  Usage:
#  Run this script without arguments to install the default versions:
#      ./install_cabocha.sh
#  Specify versions for CaboCha and CRF++:
#      ./install_cabocha.sh 0.67 0.58
#  Skip saving sources by adding a third argument:
#      ./install_cabocha.sh 0.67 0.58 -n
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `wget`, `make`, `sudo`, `apt-get`, and `tar` installed.
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

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Setup version and environment
setup_environment() {
    CABOCHA_VERSION="${1:-0.67}"
    CRF_VERSION="${2:-0.58}"
}

# Save sources if requested
save_sources() {
    sudo mkdir -p /usr/local/src/cabocha /usr/local/src/CRF
    sudo cp -a "cabocha-$CABOCHA_VERSION" /usr/local/src/cabocha
    sudo cp -a "CRF++-$CRF_VERSION" /usr/local/src/CRF
    sudo chown -R root:root /usr/local/src/cabocha /usr/local/src/CRF
}

# Install CRF++
install_crf() {
    wget "http://files.id774.net/archive/CRF%2B%2B-$CRF_VERSION.tar.gz"
    tar xzvf "CRF++-$CRF_VERSION.tar.gz"
    cd "CRF++-$CRF_VERSION" || exit 1
    ./configure
    make
    sudo make install
    cd ..
}

# Install CaboCha
install_cabocha() {
    wget "http://files.id774.net/archive/cabocha-$CABOCHA_VERSION.tar.bz2"
    tar xjvf "cabocha-$CABOCHA_VERSION.tar.bz2"
    cd "cabocha-$CABOCHA_VERSION" || exit 1
    ./configure --with-charset=UTF8 --with-posset=IPA --with-mecab-config=$(command -v mecab-config)
    make
    sudo make install
    cd ..
}

# Install CRF++ and CaboCha
install_crf_and_cabocha() {
    mkdir install_cabocha
    cd install_cabocha || exit 1
    install_crf
    install_cabocha
    cd ..
    [ -n "$3" ] || save_sources
    rm -rf install_cabocha
}

# Main execution function
main() {
    check_system
    check_commands curl wget make sudo apt-get tar
    check_network
    check_sudo
    setup_environment "$@"
    install_crf_and_cabocha "$@"
    echo "CaboCha $CABOCHA_VERSION and CRF++ $CRF_VERSION installed successfully."
}

main "$@"
