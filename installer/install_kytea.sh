#!/bin/sh

########################################################################
# install_kytea.sh: Installer for KyTea
#
#  Description:
#  This script automates the installation of KyTea by:
#  - Downloading the specified or default version from the official repository.
#  - Compiling and installing the application.
#  - Installing language bindings for Ruby and Python if available.
#  - Optionally saving the source files for future use.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v0.3 2025-03-14
#       Added network connection check, Linux system validation, command validation, and improved argument handling.
#  v0.2 2015-09-09
#       Install binding.
#  v0.1 2015-09-08
#       First.
#
#  Usage:
#  Run this script without arguments to install the default version (0.4.7):
#      ./install_kytea.sh
#  Specify a version to install a different release:
#      ./install_kytea.sh 0.5.0
#  Specify an installation prefix:
#      ./install_kytea.sh 0.5.0 /usr/local
#  Run without sudo (for local installation):
#      ./install_kytea.sh 0.5.0 ~/.local no-sudo
#  Skip saving sources by adding a third argument:
#      ./install_kytea.sh 0.5.0 /usr/local sudo -n
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `curl`, `make`, `sudo`, and `tar` installed.
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

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if [ "$SUDO" = "sudo" ] && ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access or specify 'no-sudo'." >&2
        exit 1
    fi
}

# Setup version and environment
setup_environment() {
    VERSION="${1:-0.4.7}"
    PREFIX="$2"
    if [ "$3" = "no-sudo" ]; then
        SUDO=""
    else
        SUDO="sudo"
    fi
    check_sudo
}

# Save sources if requested
save_sources() {
    if [ ! -d /usr/local/src/kytea ]; then
        sudo mkdir -p /usr/local/src/kytea
    fi
    sudo cp -a "kytea-$VERSION" /usr/local/src/kytea
}

# Install language bindings for Ruby and Python
install_binding() {
    if [ -x /opt/ruby/current/bin/gem ]; then
        $SUDO /opt/ruby/current/bin/gem install kytea
    fi
    if [ -x /opt/python/current/bin/pip ]; then
        $SUDO /opt/python/current/bin/pip install kytea
    fi
}

# Install KyTea
install_kytea() {
    setup_environment "$1" "$2" "$3"
    mkdir install_kytea
    cd install_kytea || exit 1
    curl -L -O "http://www.phontron.com/kytea/download/kytea-$VERSION.tar.gz"
    if [ ! -f "kytea-$VERSION.tar.gz" ]; then
        echo "Error: Failed to download KyTea $VERSION." >&2
        exit 1
    fi
    tar xzvf "kytea-$VERSION.tar.gz"
    cd "kytea-$VERSION" || exit 1
    if [ -z "$PREFIX" ]; then
        ./configure
    else
        ./configure --prefix="$PREFIX"
    fi
    make
    $SUDO make install
    cd ..
    [ -n "$4" ] || save_sources
    cd ..
    rm -rf install_kytea
    install_binding
}

# Main execution function
main() {
    # Perform initial checks
    check_system
    check_commands curl make sudo tar
    check_network

    # Run the installation process
    install_kytea "$1" "$2" "$3" "$4"

    echo "KyTea $VERSION installed successfully."
}

main "$@"
