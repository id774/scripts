#!/bin/sh

########################################################################
# install_mongodb.sh: Installer for MongoDB
#
#  Description:
#  This script automates the installation of MongoDB by:
#  - Downloading the specified or default version from the official repository.
#  - Setting up directories and permissions.
#  - Creating a dedicated MongoDB user.
#  - Configuring system services on Debian-based distributions.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v0.3 2025-03-14
#       Added network connection check, Linux system validation, command validation, and improved argument handling.
#       Updated to use $SCRIPTS environment variable instead of hardcoded paths.
#  v0.2 2014-10-27
#       Setup Debian services.
#  v0.1 2013-07-08
#       First version.
#
#  Usage:
#  Run this script without arguments to install the default version (2.4.8):
#      ./install_mongodb.sh
#  Specify a version to install a different release:
#      ./install_mongodb.sh 4.4.6
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `curl`, `sudo`, and `tar` installed.
#  - Must be executed in a shell environment with internet access.
#  - This script is intended for Linux systems only.
#  - The `$SCRIPTS` environment variable must be set to the directory containing necessary configuration files.
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

# Function to check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "Error: SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the directory containing MongoDB configuration files." >&2
        exit 1
    fi
}

# Setup version and environment
setup_environment() {
    VERSION="${1:-2.4.8}"
    SYSTEM_ID="linux-$(uname -m)"
    case "$OSTYPE" in
        *darwin*) OWNER="root:wheel" ;;
        *) OWNER="root:root" ;;
    esac
}

# Utility functions
mkdir_if_not_exist(){
    [ -d "$1" ] || sudo mkdir -p "$1"
}

rmdir_if_exist(){
    [ -d "$1" ] && sudo rm -rf "$1"
}

rm_if_exist_symlink(){
    [ -L "$1" ] && sudo rm -f "$1"
}

set_permission() {
    sudo chown -R "$1":"$1" /data/db
    sudo chown -R "$1":"$1" /opt/mongo/$VERSION
    sudo chmod -R g+w,o-rwx /data/db/
}

create_user() {
    sudo useradd -m "$1"
    sudo chsh -s /bin/zsh "$1"
}

create_symlink() {
    cd /opt/mongo || exit 1
    rm_if_exist_symlink current
    sudo ln -fs "$VERSION" current
}

setup_debian_service() {
    sudo cp "$SCRIPTS/etc/init.d/mongod.conf" /etc/opt/mongod.conf
    sudo chown root:root /etc/opt/mongod.conf
    sudo cp "$SCRIPTS/etc/init.d/mongod" /etc/init.d/mongod
    sudo chown root:root /etc/init.d/mongod
    sudo update-rc.d mongod defaults
    sudo /etc/init.d/mongod start
}

deploy_mongodb() {
    mkdir_if_not_exist /data/db
    mkdir_if_not_exist /opt/mongo
    rmdir_if_exist /opt/mongo/$VERSION
    sudo mv "$VERSION" /opt/mongo/
    create_user mongo
    set_permission mongo
}

install_mongodb() {
    mkdir install_mongodb
    cd install_mongodb || exit 1
    case "$SYSTEM_ID" in
      linux-i386 | linux-i686)
        curl -o mongodb.tgz "http://downloads.mongodb.org/linux/mongodb-linux-i686-$VERSION.tgz"
        tar xzvf mongodb.tgz
        mv "mongodb-linux-i686-$VERSION" "$VERSION"
        deploy_mongodb
        ;;
      linux-amd64 | linux-x86_64)
        curl -o mongodb.tgz "http://downloads.mongodb.org/linux/mongodb-linux-x86_64-$VERSION.tgz"
        tar xzvf mongodb.tgz
        mv "mongodb-linux-x86_64-$VERSION" "$VERSION"
        deploy_mongodb
        ;;
    esac
    cd ..
    rm -rf install_mongodb
}

install_main() {
    setup_environment "$1"
    install_mongodb "$1"
    [ -f /etc/debian_version ] && setup_debian_service
}

# Main function to execute the script
main() {
    # Perform initial checks
    check_system
    check_commands curl sudo tar
    check_network
    check_scripts
    check_sudo

    # Run the installation process
    install_main "$1"

    echo "MongoDB $VERSION installed successfully."
}

# Execute main function
main "$@"
