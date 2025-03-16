#!/bin/sh

########################################################################
# install_go.sh: Installer for Go (for linux-amd64)
#
#  Description:
#  This script automates the installation of Go by:
#  - Downloading the specified version from Google's official repository.
#  - Installing it to the specified target directory (default: /usr/local/go).
#  - Allowing the installation to run without sudo when specified.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-03-14
#       Added network connection check, command validation, and improved argument handling.
#  v1.0 2019-10-18
#       Stable release.
#
#  Usage:
#  Run this script with a version to install Go:
#      ./install_go.sh 1.21.0
#  Specify an installation directory:
#      ./install_go.sh 1.21.0 /opt/go
#  Run without sudo (for local installation):
#      ./install_go.sh 1.21.0 /home/user/go no-sudo
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `curl`, `tar`, and `sudo` (if using sudo) installed.
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
    if [ "$SUDO" = "sudo" ] && ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access or specify 'no-sudo'." >&2
        exit 1
    fi
}

# Setup version and environment
setup_environment() {
    VERSION="$1"
    TARGET="${2:-/usr/local/go}"
    if [ "$3" = "no-sudo" ]; then
        SUDO=""
    else
        SUDO="sudo"
    fi
    check_sudo
}

# Download and install Go
get_golang() {
    mkdir install_go
    cd install_go || exit 1
    curl -L -O "https://dl.google.com/go/go$VERSION.linux-amd64.tar.gz"
    if [ ! -f "go$VERSION.linux-amd64.tar.gz" ]; then
        echo "Error: Failed to download Go $VERSION." >&2
        exit 1
    fi
    tar xzvf "go$VERSION.linux-amd64.tar.gz"
    mv go "$VERSION"
    if [ ! -d "$TARGET" ]; then
        $SUDO mkdir -p "$TARGET"
    fi
    $SUDO mv "$VERSION" "$TARGET/"
    cd ..
    $SUDO rm -rf install_go
}

# Install Go
install_go() {
    setup_environment "$1" "$2" "$3"
    if [ -z "$VERSION" ]; then
        echo "Error: No Go version specified." >&2
        exit 1
    fi
    get_golang
    go version
}

# Main execution function
main() {
    # Perform initial checks
    check_system
    check_commands curl tar
    check_network

    # Run the installation process
    install_go "$1" "$2" "$3"

    echo "Go $VERSION installed successfully at $TARGET."
}

main "$@"
