#!/bin/sh

########################################################################
# gpg-import.sh: GPG Key Import Script for APT
#
#  Description:
#  This script imports a GPG public key from a specified keyserver and
#  adds it to the APT keyring. It's useful for adding external repository
#  keys securely. Only works on Debian-based systems.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v1.2 2024-01-18
#       Standardized command existence checks using a common function.
#  v1.1 2023-12-05
#       Added environment check for Debian-based systems.
#       Refactored for improved readability and added usage information.
#  v1.0 2008-08-22
#       Initial release. Imports GPG keys for APT from a keyserver.
#
#  Usage:
#  ./gpg-import.sh KEYSERVER PUBKEY
#
########################################################################

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

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

# Check if required commands are installed
check_commands gpg apt-key sudo

check_sudo

# Check if both arguments are provided
if [ -n "$2" ]; then
    # Import the GPG key from the specified keyserver
    gpg --keyserver "$1" --recv-keys "$2"

    # Export the GPG key and add it to the APT keyring
    sudo gpg --armor --export "$2" | sudo apt-key add -
else
    # Display usage information if arguments are missing
    echo "Usage: $0 KEYSERVER PUBKEY"
fi
