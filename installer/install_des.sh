#!/bin/sh

########################################################################
# install_des.sh: Install and Compile DES Encryption Software
#
#  Description:
#  This script downloads, compiles, and installs the DES encryption software.
#  If the software is not found in the system, it is retrieved from a remote
#  archive, extracted, compiled, and installed. Optionally, the source files
#  can be saved for future reference.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.2 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v2.1 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v2.0 2025-03-06
#       Updated to be POSIX compliant.
#       Replaced non-POSIX `which` with `command -v`.
#       Added error handling for directory changes.
#       Integrated command existence and sudo privilege checks.
#       Replaced `md5.sh` with POSIX-compliant `md5sum`.
#       Improved logging with `echo` for status updates.
#  [Intermediate versions omitted for brevity]
#  v1.0 2009-05-21
#       Derived from install_crypt.sh.
#
#  Usage:
#      ./install_des.sh [OPTION]
#
#  Options:
#  -h   Display this help message.
#  -n   Do not save source files after installation.
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

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Configure environment settings (Linux only)
setup_environment() {
    echo "Setting up environment..."
    check_system

    echo "Checking system requirements..."
    check_commands dmsetup curl wget md5sum tar make sudo rm mkdir cp chown

    echo "Checking network connectivity..."
    check_network

    check_sudo

    OWNER=root:root
}

# Save source files to /usr/local/src/crypt/des
save_sources() {
    echo "Saving source files..."
    if [ -d /usr/local/src/crypt/des ]; then
        sudo rm -rf /usr/local/src/crypt/des
    fi
    sudo mkdir -p /usr/local/src/crypt/des
    sudo cp * /usr/local/src/crypt/des
    sudo chown -R "$OWNER" /usr/local/src/crypt/des
    echo "Source files saved."
}

# Install DES encryption software
install_des() {
    echo "Setting up DES installation..."
    setup_environment

    mkdir install_des
    cd install_des || exit 1

    echo "Downloading software archive..."
    wget http://id774.net/archive/kmdes.tar.gz

    echo "Verifying archive integrity..."
    md5sum kmdes.tar.gz

    echo "Extracting files..."
    tar xzvf kmdes.tar.gz

    echo "Removing archive..."
    rm kmdes.tar.gz

    cd des || exit 1

    if [ -z "$1" ]; then
        save_sources
    fi

    echo "Compiling source code..."
    make

    echo "Installing compiled binary..."
    sudo make install

    echo "Cleaning up installation files..."
    cd .. || exit 1
    rm -rf des
    cd .. || exit 1
    rm -rf install_des
    echo "DES installation completed."
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    if command -v des >/dev/null 2>&1; then
        echo "DES is already installed."
    else
        install_des "$@"
    fi
}

# Execute main function
main "$@"
