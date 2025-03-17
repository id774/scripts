#!/bin/sh

########################################################################
# install_ncurses.sh: Installer for ncurses
#
#  Description:
#  This script automates the installation of ncurses by:
#  - Downloading the specified or default version from the GNU FTP server.
#  - Compiling and installing the package.
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
#       Refactoring.
#  v1.0 2008-08-15
#       Stable.
#
#  Usage:
#  Run this script without arguments to install the default version (5.9):
#      ./install_ncurses.sh
#  Specify a version to install a different release:
#      ./install_ncurses.sh 6.3
#  Specify an installation prefix:
#      ./install_ncurses.sh 6.3 /usr/local/ncurses
#  Skip saving sources by adding a third argument:
#      ./install_ncurses.sh 6.3 /usr/local/ncurses -n
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

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if [ "$SUDO" = "sudo" ] && ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access or specify 'no-sudo'." >&2
        exit 1
    fi
}


# Setup version and environment
setup_environment() {
    VERSION="${1:-5.9}"
    PREFIX="${2:-$HOME/local/ncurses/$VERSION}"
    if [ "$3" = "no-sudo" ]; then
        SUDO=""
    else
        SUDO="sudo"
    fi
    check_sudo
}

# Save sources if requested
save_sources() {
    if [ ! -d /usr/local/src/ncurses ]; then
        sudo mkdir -p /usr/local/src/ncurses
    fi
    sudo cp -a "ncurses-$VERSION" /usr/local/src/ncurses
}

# Install ncurses
install_ncurses() {
    setup_environment "$1" "$2" "$3"
    mkdir install_ncurses
    cd install_ncurses || exit 1
    wget "http://ftp.gnu.org/pub/gnu/ncurses/ncurses-$VERSION.tar.gz"
    if [ ! -f "ncurses-$VERSION.tar.gz" ]; then
        echo "Error: Failed to download ncurses $VERSION." >&2
        exit 1
    fi
    tar xzvf "ncurses-$VERSION.tar.gz"
    cd "ncurses-$VERSION" || exit 1
    ./configure --with-shared --with-normal --prefix="$PREFIX"
    make
    $SUDO make install
    cd ..
    [ -n "$3" ] || save_sources
    cd ..
    rm -rf install_ncurses
}

# Main function to execute the script
main() {
    # Perform initial checks
    check_system
    check_commands curl wget make sudo tar
    check_network

    # Run the installation process
    install_ncurses "$1" "$2" "$3"

    echo "ncurses $VERSION installed successfully."
}

# Execute main function
main "$@"
