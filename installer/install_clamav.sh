#!/bin/sh

########################################################################
# install_clamav.sh: Installer for ClamAV
#
#  Description:
#  This script automates the installation of ClamAV by:
#  - Cloning the latest ClamAV source from the official Git repository.
#  - Compiling and installing ClamAV with experimental features enabled.
#  - Configuring ClamAV and setting appropriate permissions.
#  - Keeping a backup of the source code for future reference (unless skipped).
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.4 2025-03-15
#       Unified structure, added system checks, improved error handling.
#  v1.3 2010-07-28
#       Make database directory if not exist.
#  v1.2 2010-03-07
#       Refactoring.
#  v1.1 2009-10-22
#       Upgrade repository svn to git.
#  v1.0 2008-08-15
#       Stable.
#
#  Usage:
#  Run this script without arguments to install ClamAV and keep source:
#      ./install_clamav.sh
#  Skip keeping source by passing any argument:
#      ./install_clamav.sh -n
#
#  Requirements:
#  - The user must have `git`, `make`, `sudo`, and `vi` installed.
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

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Setup environment
setup_environment() {
    OWNER="root:root"
}

# Keep source for future reference unless skipped
keep_source() {
    [ -n "$1" ] && return
    sudo mkdir -p /usr/local/src/security
    sudo rm -rf /usr/local/src/security/clamav /usr/local/src/security/clamav-devel
    sudo cp -R clamav-devel /usr/local/src/security
    sudo chown -R "$OWNER" /usr/local/src/security/clamav-devel
}

# Install ClamAV
install_clamav() {
    mkdir install_clamav && cd install_clamav || exit 1
    git clone git://git.clamav.net/git/clamav-devel && cd clamav-devel || exit 1

    ./configure --enable-experimental
    make
    sudo make install

    sudo vi /usr/local/etc/freshclam.conf /usr/local/etc/clamd.conf
    sudo chmod 700 /usr/local/etc/freshclam.conf
    sudo cp /usr/local/etc/freshclam.conf /usr/local/etc/freshclam.conf.base
    sudo cp /usr/local/etc/clamd.conf /usr/local/etc/clamd.conf.base

    sudo mkdir -p /usr/local/share/clamav
    sudo chown clamav:clamav /usr/local/share/clamav
    sudo freshclam

    cd .. || exit 1
    keep_source "$1"
    rm -rf clamav-devel/
    cd .. || exit 1
    rm -rf install_clamav/
}

# Main function to execute the script
main() {
    check_system
    check_commands git make sudo vi
    check_sudo
    setup_environment
    install_clamav "$1"
    echo "ClamAV installation completed successfully."
}

# Execute main function
main "$@"
