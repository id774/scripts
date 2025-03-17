#!/bin/sh

########################################################################
# install_gauche.sh: Installer for Gauche
#
#  Description:
#  This script automates the installation of Gauche by:
#  - Cloning and building the latest Gauche source (trunk version).
#  - Downloading and installing a specified stable version.
#  - Managing local and system-wide installations with sudo support.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-03-15
#       Unified structure, added system checks, improved error handling.
#  v0.1 2012-05-16
#       First.
#
#  Usage:
#  Install the trunk version of Gauche:
#      ./install_gauche.sh trunk
#  Install a specific stable version (e.g., 0.9.3.2):
#      ./install_gauche.sh 0.9.3.2
#  Specify an installation prefix:
#      ./install_gauche.sh 0.9.3.2 /opt/gauche
#  Disable sudo by passing a third argument:
#      ./install_gauche.sh 0.9.3.2 /opt/gauche -n
#
#  Requirements:
#  - The user must have `git`, `make`, `sudo`, and `wget` installed.
#  - Network connectivity is required to download the source.
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

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Setup environment
setup_environment() {
    SUDO="sudo"
    [ -n "$3" ] && SUDO=""
    OWNER="root:root"
    [ "$SUDO" = "sudo" ] && check_sudo
}

# Clone or update Gauche repository
gitpull() {
    echo "Pulling Gauche source..."
    mkdir -p "$HOME/local/git"
    cd "$HOME/local/git" || exit 1
    [ -d Gauche ] && (cd Gauche && git pull) || git clone git://gauche.git.sourceforge.net/gitroot/gauche/Gauche
}

# Build and install Gauche
make_and_install() {
    ./DIST gen
    ./configure --prefix="${2:-$HOME/local/gauche/trunk}"
    make
    $SUDO make install
}

# Install trunk version
install_trunk() {
    gitpull
    cd "$HOME/local/git/Gauche" || exit 1
    make_and_install "$@"
}

# Install stable version
install_stable() {
    mkdir install_gauche
    cd install_gauche || exit 1
    wget "http://prdownloads.sourceforge.net/gauche/Gauche-$1.tgz"
    tar xzvf "Gauche-$1.tgz"
    cd "Gauche-$1" || exit 1
    make_and_install "$@"
    cd ..
    $SUDO mkdir -p /usr/local/src/gauche
    $SUDO cp -a "Gauche-$1" /usr/local/src/gauche
    $SUDO chown -R "$OWNER" "/usr/local/src/gauche/Gauche-$1"
    cd ..
    $SUDO rm -rf install_gauche
}

# Main function to execute the script
main() {
    check_system
    check_commands curl git make sudo wget tar
    check_network
    setup_environment "$@"
    case "$1" in
      trunk)
        install_trunk "$@"
        ;;
      *)
        install_stable "$@"
        ;;
    esac
    echo "Gauche installation completed successfully."
}

# Execute main function
main "$@"
