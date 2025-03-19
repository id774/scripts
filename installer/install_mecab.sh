#!/bin/sh

########################################################################
# install_mecab.sh: Installer for MeCab and related dictionaries
#
#  Description:
#  This script automates the installation of MeCab by:
#  - Downloading and compiling the specified or default version.
#  - Installing the IPADIC and NAIST dictionaries.
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
#  v0.4 2015-05-29
#       Change URL, fix args bug.
#  v0.3 2014-02-10
#       Refactoring.
#  v0.2 2013-01-28
#       Add NAIST dic.
#  v0.1 2012-08-07
#       First.
#
#  Usage:
#  Run this script without arguments to install the default version:
#      ./install_mecab.sh
#  Specify versions for MeCab, IPADIC, and NAIST dictionaries:
#      ./install_mecab.sh 0.996 2.7.0-20070801 53500
#  Skip saving sources by adding a fourth argument:
#      ./install_mecab.sh 0.996 2.7.0-20070801 53500 -n
#
#  Requirements:
#  - The user must have `wget`, `tar`, `make`, and `sudo` installed.
#  - Network connectivity is required to download the source files.
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

# Setup environment
setup_environment() {
    MECAB_VERSION="${1:-0.996}"
    IPADIC_VERSION="${2:-2.7.0-20070801}"
    NAISTDIC_VERSION="${3:-53500}"
}

# Install MeCab
install_mecab() {
    mkdir install_mecab
    cd install_mecab || exit 1

    wget "http://files.id774.net/archive/mecab-$MECAB_VERSION.tar.gz"
    tar xzvf "mecab-$MECAB_VERSION.tar.gz"
    cd "mecab-$MECAB_VERSION" || exit 1
    ./configure --enable-utf8-only
    make
    sudo make install
    cd .. || exit 1

    wget "http://files.id774.net/archive/mecab-ipadic-$IPADIC_VERSION.tar.gz"
    tar xzvf "mecab-ipadic-$IPADIC_VERSION.tar.gz"
    cd "mecab-ipadic-$IPADIC_VERSION" || exit 1
    ./configure --with-charset=utf8
    make
    sudo make install
    cd .. || exit 1

    wget "http://files.id774.net/archive/naistdic.tar.gz"
    tar xzvf "naistdic.tar.gz"
    cd "mecab-naist-jdic-0.6.3b-20111013" || exit 1

    ./configure --with-charset=utf8
    make
    sudo make install
    cd .. || exit 1

    [ -n "$4" ] || save_sources
    cd .. || exit 1
    rm -rf install_mecab
}

# Save sources if requested
save_sources() {
    sudo mkdir -p /usr/local/src/mecab
    sudo cp -a mecab-$MECAB_VERSION mecab-ipadic-$IPADIC_VERSION "mecab-naist-jdic-0.6.3b-20111013" /usr/local/src/mecab
    sudo chown -R root:root /usr/local/src/mecab
}

# Main function to execute the script
main() {
    check_system
    check_commands curl wget tar make sudo
    check_network
    check_sudo
    setup_environment "$@"
    install_mecab "$@"
    echo "MeCab installation completed successfully."
}

# Execute main function
main "$@"
