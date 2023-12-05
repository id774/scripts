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

# Check if required commands are available
if ! command -v apt-key >/dev/null 2>&1; then
    echo "Error: apt-key is not available. This script is intended for Debian-based systems."
    exit 1
fi

if ! command -v gpg >/dev/null 2>&1; then
    echo "Error: gpg is not installed. This script only works on systems with gpg."
    exit 1
fi

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

