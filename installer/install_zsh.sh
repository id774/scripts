#!/bin/sh

########################################################################
# install_zsh.sh: Installer for Zsh
#
#  Description:
#  This script automates the installation of Zsh by:
#  - Downloading the specified or default version from SourceForge.
#  - Compiling and installing the package.
#  - Optionally saving the source files for future use.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v0.4 2025-03-14
#       Added network connection check, system validation, command validation, and improved argument handling.
#  v0.3 2014-01-19
#       Update to zsh 5.0.5, Change source URL.
#  v0.2 2010-09-16
#       Refactoring.
#  v0.1 2010-09-14
#       First version.
#
#  Usage:
#  Run this script without arguments to install the default version (5.0.5):
#      ./install_zsh.sh
#  Specify a version to install a different release:
#      ./install_zsh.sh 5.9
#  Specify an installation prefix:
#      ./install_zsh.sh 5.9 /opt/zsh
#  Skip saving sources by adding a third argument:
#      ./install_zsh.sh 5.9 /opt/zsh -n
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

# Function to check if user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Setup version and environment
setup_environment() {
    ZSH_VERSION="${1:-5.0.5}"
    SUDO="${3:-sudo}"
    case $OSTYPE in
      *darwin*)
        OPTIONS=-pR
        OWNER=root:wheel
        ;;
      *)
        OPTIONS=-a
        OWNER=root:root
        ;;
    esac
}

# Save sources if requested
save_sources() {
    sudo mkdir -p /usr/local/src/zsh
    sudo cp $OPTIONS "zsh-$ZSH_VERSION" /usr/local/src/zsh
    sudo chown $OWNER /usr/local/src/zsh
    sudo chown -R $OWNER /usr/local/src/zsh/zsh-$ZSH_VERSION
}

# Compile and install Zsh
make_and_install() {
    cd "zsh-$ZSH_VERSION" || exit 1
    ./configure --enable-multibyte --prefix="${2:-/usr/local}"
    make
    $SUDO make install
    cd ..
}

# Download and extract Zsh
install_zsh() {
    mkdir install_zsh
    cd install_zsh || exit 1
    wget "http://sourceforge.net/projects/zsh/files/zsh/$ZSH_VERSION/zsh-$ZSH_VERSION.tar.gz/download" -O "zsh-$ZSH_VERSION.tar.gz"
    if [ ! -f "zsh-$ZSH_VERSION.tar.gz" ]; then
        echo "Error: Failed to download Zsh $ZSH_VERSION." >&2
        exit 1
    fi
    tar xzvf "zsh-$ZSH_VERSION.tar.gz"
    [ "$2" = "sourceonly" ] || make_and_install "$1" "$2"
    [ -n "$3" ] || save_sources
    cd ..
    rm -rf install_zsh
}

# Main execution function
main() {
    # Perform initial checks
    check_system
    check_commands curl wget make sudo tar
    check_network
    check_sudo

    # Run the installation process
    setup_environment "$1" "$2" "$3"
    install_zsh "$1" "$2" "$3"

    echo "Zsh $ZSH_VERSION installed successfully."
}

main "$@"
