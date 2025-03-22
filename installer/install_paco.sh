#!/bin/sh

########################################################################
# install_paco.sh: Installer for paco
#
#  Description:
#  This script automates the installation of paco by:
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
#  v1.4 2025-03-14
#       Added network connection check, Linux system validation, command validation, and improved argument handling.
#  v1.3 2010-09-16
#       Refactoring.
#  v1.2 2010-03-07
#       Refactoring and update to 2.0.7.
#  v1.1 2008-12-15
#       Keep sources.
#  v1.0 2008-12-04
#       Stable.
#
#  Usage:
#  Run this script without arguments to install the default version (2.0.9):
#      ./install_paco.sh
#  Specify a version to install a different release:
#      ./install_paco.sh 2.0.8
#  Skip saving sources by adding a second argument:
#      ./install_paco.sh 2.0.9 -n
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `wget`, `make`, `sudo`, and `tar` installed.
#  - Must be executed in a shell environment with internet access.
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
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Setup version and environment
setup_environment() {
    PACO_VERSION="${1:-2.0.9}"
}

# Save sources if requested
save_sources() {
    sudo mkdir -p /usr/local/src/paco
    sudo cp -av "paco-$PACO_VERSION" /usr/local/src/paco/
    sudo chown -R root:root /usr/local/src/paco
}

# Install paco
install_paco() {
    setup_environment "$1"
    mkdir install_paco
    cd install_paco || exit 1
    wget "http://downloads.sourceforge.net/paco/paco-$PACO_VERSION.tar.gz"
    if [ ! -f "paco-$PACO_VERSION.tar.gz" ]; then
        echo "Error: Failed to download paco $PACO_VERSION." >&2
        exit 1
    fi
    tar xzvf "paco-$PACO_VERSION.tar.gz"
    cd "paco-$PACO_VERSION" || exit 1
    ./configure --disable-gpaco
    make
    sudo make install
    sudo make logme
    cd .. || exit 1
    [ -n "$2" ] || save_sources
    cd .. || exit 1
    rm -rf install_paco
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    # Perform initial checks
    check_system
    check_commands curl wget make sudo tar mkdir chown cp rm
    check_network
    check_sudo

    # Run the installation process
    install_paco "$1" "$2"

    echo "paco $PACO_VERSION installed successfully."
}

# Execute main function
main "$@"
