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
#  v1.0 2025-03-20
#       Added network connection check, system validation, command validation, and improved argument handling.
#       Default zsh version 5.9 installs in '/opt/zsh/5.9' directory.
#       Improved directory navigation safety.
#       Set default installation path to /opt/zsh/x.x.
#  [Further version history truncated for brevity]
#  v0.1 2010-09-14
#       First version.
#
#  Usage:
#  Run this script without arguments to install the default version (5.9):
#      ./install_zsh.sh
#  Specify a version to install a different release:
#      ./install_zsh.sh 5.8.1
#  Specify an installation prefix:
#      ./install_zsh.sh 5.8.1 /usr/local
#  Run without sudo (for local installation):
#      ./install_zsh.sh 5.9 ~/.local/zsh --no-sudo
#  Skip saving sources by adding a fourth argument:
#      ./install_zsh.sh 5.9 /opt/zsh sudo -n
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `wget`, `make`, `sudo`, and `tar` installed.
#  - Must be executed in a shell environment with internet access.
#
########################################################################

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
    VERSION="${1:-5.9}"
    MAJOR_MINOR="$(echo "$VERSION" | awk -F. '{print $1"."$2}')"
    PREFIX="${2:-/opt/zsh/$MAJOR_MINOR}"

    if [ -z "$3" ] || [ "$3" = "sudo" ]; then
        SUDO="sudo"
    else
        SUDO=""
    fi
    [ "$SUDO" = "sudo" ] && check_sudo

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
    $SUDO mkdir -p /usr/local/src/zsh
    $SUDO cp $OPTIONS "zsh-$VERSION" /usr/local/src/zsh
    $SUDO chown $OWNER /usr/local/src/zsh
    $SUDO chown -R $OWNER /usr/local/src/zsh/zsh-$VERSION
}

# Compile and install Zsh
make_and_install() {
    cd "zsh-$VERSION" || exit 1
    ./configure --enable-multibyte --prefix="$PREFIX"
    make
    $SUDO make install
    cd ..
}

# Download and extract Zsh
install_zsh() {
    mkdir install_zsh
    cd install_zsh || exit 1

    if [ "$USER_SPECIFIED_VERSION" -eq 1 ]; then
        ZSH_URL="https://www.zsh.org/pub/old/zsh-$VERSION.tar.xz"
    else
        ZSH_URL="https://www.zsh.org/pub/zsh-$VERSION.tar.xz"
    fi

    wget "$ZSH_URL"
    if [ ! -f "zsh-$VERSION.tar.xz" ]; then
        echo "Error: Failed to download Zsh $VERSION." >&2
        exit 1
    fi
    tar xvf "zsh-$VERSION.tar.xz"
    make_and_install
    [ -n "$4" ] || save_sources
    cd ..
    rm -rf install_zsh
}

# Main function to execute the script
main() {
    # Perform initial checks
    check_commands curl wget make sudo tar awk mkdir cp chown uname
    check_network

    USER_SPECIFIED_VERSION=0
    if [ -n "$1" ]; then
        USER_SPECIFIED_VERSION=1
    fi

    # Run the installation process
    setup_environment "$@"
    install_zsh "$VERSION" "$PREFIX" "$SUDO" "$4"

    echo "Zsh $VERSION installed successfully in $PREFIX."
}

# Execute main function
main "$@"
