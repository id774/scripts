#!/bin/sh

########################################################################
# install_dotvim.sh: Installer for dot_vim Configuration
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
#      ./install_dotvim.sh
#  Specify a custom installation path:
#      ./install_dotvim.sh /custom/path/to/.vim
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

# Function to check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "Error: SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the directory containing the dot_vim configuration files." >&2
        exit 1
    fi
}

# Set up environment variables
setup_environment() {
    TARGET="${1:-$HOME/.vim}"
    case "$(uname -s)" in
        Darwin)
            OPTIONS="-Rv"
            ;;
        *)
            OPTIONS="-Rvd"
            ;;
    esac
}

# Install dot_vim configuration
install_dotvim() {
    if [ ! -d "$TARGET" ]; then
        mkdir -p "$TARGET"
    fi
    cp $OPTIONS "$SCRIPTS/dot_files/dot_vim"/* "$TARGET"/
}

# Main function to execute the script
main() {
    # Perform initial checks
    check_commands cp mkdir vim uname
    check_scripts

    # Ensure dot_vim source exists before proceeding
    if [ ! -d "$SCRIPTS/dot_files/dot_vim" ]; then
        echo "Error: dot_vim source directory does not exist. Ensure that the SCRIPTS variable is correctly set." >&2
        exit 1
    fi

    setup_environment "$1"
    install_dotvim "$1"

    echo "dot_vim configuration installed successfully at $TARGET."
}

# Execute main function
main "$@"
