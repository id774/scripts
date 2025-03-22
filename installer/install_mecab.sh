#!/bin/sh

########################################################################
# install_mecab.sh: Installer for MeCab and related dictionaries
#
#  Description:
#  This script automates the installation of MeCab by:
#  - Installing the IPADIC and NAIST dictionaries.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-20
#       Unified structure, added system checks, improved error handling.
#       Removed arguments, fixed versions, and removed source saving.
#  [Further version history truncated for brevity]
#  v0.1 2012-08-07
#       First version.
#
#  Usage:
#  Run this script without arguments to install the default version:
#      ./install_mecab.sh
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - This script is intended for Linux systems only.
#
########################################################################

# Display script usage information
usage() {
    awk '
        BEGIN { in_usage = 0 }
        /^#  Usage:/ { in_usage = 1; print substr($0, 4); next }
        /^#{10}/ { if (in_usage) exit }
        in_usage && /^#/ { print substr($0, 4) }
    ' "$0"
    exit 0
}

# Fixed versions
MECAB_VERSION="0.996"
IPADIC_VERSION="2.7.0-20070801"
NAISTDIC_VERSION="0.6.3b-20111013"

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

# Install MeCab and dictionaries
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
    cd "mecab-naist-jdic-$NAISTDIC_VERSION" || exit 1
    ./configure --with-charset=utf8
    make
    sudo make install
    cd .. || exit 1

    cd .. || exit 1
    rm -rf install_mecab
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    check_commands curl wget tar make sudo
    check_network
    check_sudo
    install_mecab
    echo "MeCab installation completed successfully."
}

# Execute main function
main "$@"
