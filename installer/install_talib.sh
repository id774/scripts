#!/bin/sh

########################################################################
# install_talib.sh: Installer for TA-Lib
#
#  Description:
#  This script automates the installation of TA-Lib by:
#  - Downloading the specified or default version from the archive site.
#  - Compiling and installing the package.
#  - Optionally saving the source files for future use.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v0.4 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v0.3 2025-03-14
#       Added sudo privilege check, system validation, command validation, and improved argument handling.
#  v0.2 2015-05-31
#       Change URL.
#  v0.1 2015-03-23
#       First version.
#
#  Usage:
#  Run this script without arguments to install the default version (0.4.0):
#      ./install_talib.sh
#  Specify a version to install a different release:
#      ./install_talib.sh 0.4.1
#  Skip saving sources by adding a second argument:
#      ./install_talib.sh 0.4.1 -n
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `curl`, `make`, `sudo`, and `tar` installed.
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

# Function to check if user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Setup version and environment
setup_environment() {
    VERSION="${1:-0.4.0}"
    FILENAME="ta-lib-$VERSION-src.tar.gz"
}

# Save sources if requested
save_sources() {
    sudo mkdir -p /usr/local/src
    sudo cp -av ta-lib /usr/local/src/
    sudo chown -R root:root /usr/local/src/
}

# Install TA-Lib
install_talib() {
    setup_environment "$1"
    mkdir install_talib
    cd install_talib || exit 1
    curl -L "http://files.id774.net/archive/$FILENAME" -O
    if [ ! -f "$FILENAME" ]; then
        echo "Error: Failed to download TA-Lib $VERSION." >&2
        exit 1
    fi
    tar xzvf "$FILENAME"
    cd ta-lib || exit 1
    ./configure --prefix=/usr/local
    make
    sudo make install
    cd .. || exit 1
    [ -n "$2" ] || save_sources
    cd .. || exit 1
    rm -rf install_talib
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    # Perform initial checks
    check_system
    check_commands curl make sudo tar
    check_network
    check_sudo

    # Run the installation process
    install_talib "$1" "$2"

    echo "TA-Lib $VERSION installed successfully."
}

# Execute main function
main "$@"
