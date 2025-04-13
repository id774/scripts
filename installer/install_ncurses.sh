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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v2.0 2025-03-19
#       Added network connection check, Linux system validation, command validation, and improved argument handling.
#  [Further version history truncated for brevity]
#  v1.0 2008-08-15
#       Stable.
#
#  Usage:
#  Run this script without arguments to install the default version (6.5):
#      ./install_ncurses.sh
#  Specify a version to install a different release:
#      ./install_ncurses.sh 5.9
#  Specify an installation prefix:
#      ./install_ncurses.sh 6.5 /opt/ncurses/6.5
#  Run without sudo (for local installation):
#      ./install_ncurses.sh 5.9 ~/.local/ncurses --no-sudo
#  Skip saving sources by adding a fourth argument:
#      ./install_ncurses.sh 6.5 /usr/local/ncurses sudo -n
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `wget`, `make`, and `tar` installed.
#  - Must be executed in a shell environment with internet access.
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
    if [ "$SUDO" = "sudo" ] && ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access or specify 'no-sudo'." >&2
        exit 1
    fi
}

# Setup version and environment
setup_environment() {
    VERSION="${1:-6.5}"
    MAJOR_MINOR="$(echo "$VERSION" | awk -F. '{print $1"."$2}')"
    PREFIX="${2:-/opt/ncurses/$MAJOR_MINOR}"
    if [ -z "$3" ] || [ "$3" = "sudo" ]; then
        SUDO="sudo"
    else
        SUDO=""
    fi
    check_sudo
    DOWNLOAD_SOURCE="${4:-auto}"

    case "$(uname -s)" in
        Darwin)
            OPTIONS="-pR"
            OWNER="root:wheel"
            ;;
        *)
            OPTIONS="-a"
            OWNER="root:root"
            ;;
    esac
}

# Save sources if requested
save_sources() {
    [ "$SUDO" = "sudo" ] || return
    if [ ! -d /usr/local/src/ncurses ]; then
        $SUDO mkdir -p /usr/local/src/ncurses
    fi
    $SUDO cp $OPTIONS "ncurses-$VERSION" /usr/local/src/ncurses
}

# Install ncurses
install_ncurses() {
    mkdir install_ncurses
    cd install_ncurses || exit 1
    wget "http://ftp.gnu.org/pub/gnu/ncurses/ncurses-$VERSION.tar.gz"

    # Check if the file was downloaded successfully
    if [ ! -f "ncurses-$VERSION.tar.gz" ]; then
        echo "[ERROR] Failed to download ncurses $VERSION." >&2
        exit 1
    fi

    tar xzvf "ncurses-$VERSION.tar.gz"
    cd "ncurses-$VERSION" || exit 1
    ./configure --with-shared --with-normal --prefix="$PREFIX"
    make
    $SUDO make install
    cd .. || exit 1
    [ "$DOWNLOAD_SOURCE" = "auto" ] && save_sources
    cd .. || exit 1
    $SUDO rm -rf install_ncurses
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    # Perform initial checks
    check_commands curl wget make sudo tar awk mkdir cp uname
    check_network

    # Run the installation process
    setup_environment "$@"
    install_ncurses

    echo "[INFO] ncurses $VERSION installed successfully."
}

# Execute main function
main "$@"
