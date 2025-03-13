#!/bin/sh
#
########################################################################
# Environment for Debian
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.3 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v0.2 2012-04-16
#       Shape up unnecessary functions.
#  v0.1 2011-09-28
#       First version.
########################################################################

# Check if the system supports apt-get
check_environment() {
    if ! command -v apt-get >/dev/null 2>&1; then
        echo "Error: apt-get is not available on this system. This script requires a Debian-based environment."
        exit 1
    fi
}

setup_environment() {
    SCRIPTS="$HOME/scripts"
    if [ ! -d "$SCRIPTS" ]; then
        echo "Error: Directory '$SCRIPTS' does not exist. Please create it or specify the correct path."
        exit 1
    fi
}

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again."
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions."
            exit 126
        fi
    done
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access."
        exit 1
    fi
}

apt_upgrade() {
    sudo apt-get update &&
    sudo apt-get -y upgrade &&
    sudo apt-get autoclean &&
    sudo apt-get -y autoremove
}

create_admin_group() {
    sudo groupadd admin
    sudo groupadd wheel
}

setup_tune2fs() {
    $SCRIPTS/installer/setup_tune2fs.sh
}

# Main operation
main() {
    check_environment
    setup_environment
    check_commands sudo vi
    check_sudo

    sudo vi /etc/apt/sources.list
    apt_upgrade

    setup_tune2fs
}

main "$@"
