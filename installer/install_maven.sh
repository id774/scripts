#!/bin/sh

########################################################################
# install_maven.sh: Installer for Apache Maven
#
#  Description:
#  This script automates the installation of Apache Maven by:
#  - Downloading the specified or default version from an official mirror.
#  - Extracting and installing it to /opt/maven.
#  - Setting appropriate ownership based on OS type.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v0.2 2025-03-14
#       Added network connection check, Linux system validation, command validation, and improved argument handling.
#  v0.1 2012-12-06
#       First version.
#
#  Usage:
#  Run this script without arguments to install the default version (3.0.4):
#      ./install_maven.sh
#  Specify a version to install a different release:
#      ./install_maven.sh 3.8.6
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `wget`, `sudo`, and `tar` installed.
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
    VERSION="${1:-3.0.4}"
    case "$OSTYPE" in
        *darwin*) OWNER="root:wheel" ;;
        *) OWNER="root:root" ;;
    esac
}

# Install Maven
install_maven() {
    setup_environment "$1"
    mkdir install_maven
    cd install_maven || exit 1
    wget "http://ftp.jaist.ac.jp/pub/apache/maven/maven-3/$VERSION/binaries/apache-maven-$VERSION-bin.tar.gz"
    if [ ! -f "apache-maven-$VERSION-bin.tar.gz" ]; then
        echo "Error: Failed to download Maven $VERSION." >&2
        exit 1
    fi
    tar xzvf "apache-maven-$VERSION-bin.tar.gz"
    if [ -d "/opt/maven/$VERSION" ]; then
        sudo rm -rf "/opt/maven/$VERSION"
    fi
    sudo mkdir -p "/opt/maven/$VERSION"
    sudo mv apache-maven-$VERSION/* "/opt/maven/$VERSION"
    sudo chown -R $OWNER "/opt/maven/$VERSION"
    cd ..
    rm -rf install_maven
}

# Main execution function
main() {
    # Perform initial checks
    check_system
    check_commands curl wget sudo tar
    check_network
    check_sudo

    # Run the installation process
    install_maven "$1"

    echo "Apache Maven $VERSION installed successfully."
}

main "$@"
