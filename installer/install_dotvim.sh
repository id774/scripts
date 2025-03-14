#!/bin/sh

########################################################################
# install_dot_vim.sh: Installer for dot_vim Configuration
#
#  Description:
#  This script automates the setup of the `.vim` directory by:
#  - Ensuring the required target directory exists.
#  - Copying configuration files from the `dot_vim` repository.
#  - Adjusting options based on the operating system type.
#  - Allowing a custom installation path via command-line argument.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-03-14
#       Added command existence check and environment variable validation.
#  v1.0 2010-03-07
#       First version.
#
#  Usage:
#  Run this script directly without any arguments for default installation:
#      ./install_dot_vim.sh
#  Specify a custom installation path:
#      ./install_dot_vim.sh /custom/path/to/.vim
#
#  Requirements:
#  - The `SCRIPTS` environment variable must be set to the directory
#    containing the `dot_vim` configuration files.
#  - Must be executed in a shell environment where `cp` and `vim` are available.
#
########################################################################

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

# Check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "Error: SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the directory containing the dot_vim configuration files." >&2
        exit 1
    fi
}

# Perform initial checks
check_commands cp mkdir vim
check_scripts

# Set up environment variables
setup_environment() {
    TARGET="${1:-$HOME/.vim}"
    case "$OSTYPE" in
        *darwin*) OPTIONS="-Rv" ;;
        *) OPTIONS="-Rvd" ;;
    esac
}

# Install dot_vim configuration
install_dotvim() {
    setup_environment "$1"
    if [ ! -d "$TARGET" ]; then
        mkdir -p "$TARGET"
    fi
    cp $OPTIONS "$SCRIPTS/dot_files/dot_vim"/* "$TARGET"/
}

# Ensure dot_vim source exists before proceeding
if [ ! -d "$SCRIPTS/dot_files/dot_vim" ]; then
    echo "Error: dot_vim source directory does not exist. Ensure that the SCRIPTS variable is correctly set." >&2
    exit 1
fi

install_dotvim "$1"

echo "dot_vim configuration installed successfully at $TARGET."
