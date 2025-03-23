#!/bin/sh

########################################################################
# install_neologd.sh: Installer for mecab-ipadic-neologd
#
#  Description:
#  This script automates the installation of mecab-ipadic-neologd by:
#  - Cloning the latest source from the official repository.
#  - Running the installation script.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-16
#       Unified structure, added system checks, improved error handling.
#  v0.1 2016-05-09
#       First.
#
#  Usage:
#  Run this script to install mecab-ipadic-neologd:
#      ./install_neologd.sh
#
#  Requirements:
#  - The user must have `git` installed.
#  - Network connectivity is required to clone the repository.
#  - This script is intended for Linux systems only.
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

# Install mecab-ipadic-neologd
install_neologd() {
    TMPDIR="${TMP:-/tmp}"
    cd "$TMPDIR" || exit 1
    git clone --depth 1 https://github.com/neologd/mecab-ipadic-neologd.git
    cd mecab-ipadic-neologd || exit 1
    ./bin/install-mecab-ipadic-neologd
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    check_commands curl git
    check_network
    install_neologd
    echo "mecab-ipadic-neologd installation completed successfully."
}

# Execute main function
main "$@"
