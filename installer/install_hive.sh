#!/bin/sh

########################################################################
# install_hive.sh: Installer for Apache Hive
#
#  Description:
#  This script automates the installation of Apache Hive by:
#  - Downloading the specified or default version from an official mirror.
#  - Extracting and installing it to /opt/hive.
#  - Setting appropriate ownership and permissions based on OS type.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v0.2 2025-03-14
#       Added network connection check, Linux system validation, command validation, and improved argument handling.
#  v0.1 2012-07-30
#       First version.
#
#  Usage:
#  Run this script without arguments to install the default version (0.9.0):
#      ./install_hive.sh
#  Specify a version to install a different release:
#      ./install_hive.sh 3.1.2
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `wget`, `tar`, and `sudo` installed.
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

# Setup version and environment
setup_environment() {
    VERSION="${1:-0.9.0}"
    HIVE="hive-$VERSION"
    case "$OSTYPE" in
        *darwin*) OWNER="root:wheel"; OPTIONS="-pR" ;;
        *) OWNER="root:root"; OPTIONS="-a" ;;
    esac
}

# Download and install Hive
install_hive() {
    setup_environment "$1"
    mkdir install_hive
    cd install_hive || exit 1
    wget "http://ftp.tsukuba.wide.ad.jp/software/apache/hive/$HIVE/$HIVE.tar.gz"
    if [ ! -f "$HIVE.tar.gz" ]; then
        echo "Error: Failed to download Hive $VERSION." >&2
        exit 1
    fi
    tar xzvf "$HIVE.tar.gz"
    rm "$HIVE.tar.gz"
    if [ -d "/opt/hive/$VERSION" ]; then
        sudo rm -rf "/opt/hive/$VERSION"
    fi
    if [ ! -d "/opt/hive" ]; then
        sudo mkdir -p "/opt/hive"
    fi
    sudo cp $OPTIONS "$HIVE" "/opt/hive/$VERSION"
    sudo chown -R $OWNER "/opt/hive/$VERSION"
    cd ..
    rm -rf install_hive
}

# Perform initial checks
check_system
check_commands wget tar ping
check_network

# Run the installation process
install_hive "$1"

echo "Apache Hive $VERSION installed successfully."
