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
#      ./install_zsh.sh 5.0.5 /usr/local
#  Run without sudo (for local installation):
#      ./install_zsh.sh 5.0.5 ~/.local/zsh --no-sudo
#  Skip saving sources by adding a fourth argument:
#      ./install_zsh.sh 5.0.5 /opt/zsh sudo -n
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
    ZSH_VERSION="${1:-5.0.5}"
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
    $SUDO cp $OPTIONS "zsh-$ZSH_VERSION" /usr/local/src/zsh
    $SUDO chown $OWNER /usr/local/src/zsh
    $SUDO chown -R $OWNER /usr/local/src/zsh/zsh-$ZSH_VERSION
}

# Compile and install Zsh
make_and_install() {
    cd "zsh-$ZSH_VERSION" || exit 1
    ./configure --enable-multibyte --prefix="$PREFIX"
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
    [ -n "$4" ] || save_sources
    cd ..
    rm -rf install_zsh
}

# Main function to execute the script
main() {
    # Perform initial checks
    check_commands curl wget make sudo tar awk mkdir cp chown uname
    check_network

    # Run the installation process
    setup_environment "$@"
    install_zsh "$VERSION" "$PREFIX" "$SUDO" "$4"

    echo "Zsh $ZSH_VERSION installed successfully in $PREFIX."
}

# Execute main function
main "$@"
