#!/bin/sh

########################################################################
# install_resin.sh: Installer for Resin
#
#  Description:
#  This script automates the installation of Resin by:
#  - Downloading the specified or default version from the official site.
#  - Compiling and installing the application.
#  - Optionally saving the source files for future use.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v0.5 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v0.4 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v0.3 2025-03-14
#       Added network connection check, Linux system validation, command validation, and improved argument handling.
#  v0.2 2011-06-22
#       Version added to installation target.
#  v0.1 2011-06-16
#       First version.
#
#  Usage:
#  Run this script without arguments to install the default version (4.0.67):
#      ./install_resin.sh
#  Specify a version to install a different release:
#      ./install_resin.sh 4.0.66
#  Skip saving sources by adding a second argument:
#      ./install_resin.sh 4.0.66 -n
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `wget`, `make`, `sudo`, `unzip`, and `tar` installed.
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
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Function to check network connectivity
check_network() {
    if ! curl -s --head --connect-timeout 5 http://clients3.google.com/generate_204 >/dev/null; then
        echo "[ERROR] No network connection detected. Please check your internet access." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Setup version and environment
setup_environment() {
    VERSION="${1:-4.0.67}"
    RESIN="resin-$VERSION"
}

# Save sources if requested
save_sources() {
    sudo mkdir -p /usr/local/src/resin
    sudo cp -av "$RESIN" /usr/local/src/resin/
    sudo chown -R root:root /usr/local/src/resin
}

# Install Resin
install_resin() {
    setup_environment "$1"
    mkdir install_resin
    cd install_resin || exit 1
    wget "http://www.caucho.com/download/$RESIN.zip"
    if [ ! -f "$RESIN.zip" ]; then
        echo "[ERROR] Failed to download Resin $VERSION." >&2
        exit 1
    fi
    unzip "$RESIN.zip"
    cd "$RESIN" || exit 1
    ./configure --prefix="/opt/resin/$VERSION"
    make
    sudo make install
    cd .. || exit 1
    [ -n "$2" ] || save_sources
    cd .. || exit 1
    rm -rf install_resin
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    # Perform initial checks
    check_system
    check_commands curl wget make sudo unzip tar chown mkdir cp rm
    check_network
    check_sudo

    # Run the installation process
    install_resin "$1" "$2"

    echo "[INFO] Resin $VERSION installed successfully."
}

# Execute main function
main "$@"
