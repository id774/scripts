#!/bin/sh

########################################################################
# debian_init.sh: Initial Setup Script for Ubuntu/Debian
#
#  Description:
#  This script automates the initial setup of Debian-based systems, ensuring
#  a consistent environment configuration. It verifies the system type,
#  sets up necessary directories, and executes predefined installation scripts.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v5.0 2025-03-13
#       Improved system detection for Debian and Ubuntu.
#       Enhanced documentation and comments for better maintainability.
#       Ensured strict POSIX compliance.
#       Redirected error messages to stderr for better logging and debugging.
#  [Further version history truncated for brevity]
#  v0.1 2007-08-27
#       First version.
#
#  Usage:
#  Run the script directly:
#      ./debian_init.sh
#  Ensure that the required setup scripts exist in the designated directory.
#  This script verifies system compatibility before proceeding.
#
#  Notes:
#  - The script is designed for Debian-based systems (Debian, Ubuntu, etc.).
#  - Internet connectivity is required for package installations.
#  - Review and modify the installation scripts as needed before execution.
#
#  Error Conditions:
#  - If the system is not Debian-based, the script exits with an error.
#  - If the required directory does not exist, an error message is displayed.
#  - Errors from underlying scripts should be resolved based on their output.
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

# Function to verify if the system is Debian-based
check_debian_based() {
    if [ ! -f /etc/os-release ]; then
        echo "Error: Unable to determine the operating system." >&2
        exit 1
    fi

    # Extracting OS information
    OS_ID=$(awk -F= '/^ID=/{print $2}' /etc/os-release | tr -d '"')
    OS_LIKE=$(awk -F= '/^ID_LIKE=/{print $2}' /etc/os-release | tr -d '"')

    # Checking if the system is Debian-based
    case "$OS_ID" in
        debian|ubuntu) return 0 ;;
        *)
            case "$OS_LIKE" in
                *debian*) return 0 ;;
                *)
                    echo "Error: This script is intended for Debian or Ubuntu systems only." >&2
                    exit 1
                    ;;
            esac
            ;;
    esac
}

# Function to verify script environment
setup_environment() {
    SCRIPTS="$HOME/scripts"
    if [ ! -d "$SCRIPTS" ]; then
        echo "Error: Directory '$SCRIPTS' does not exist. Please create it or specify the correct path." >&2
        exit 1
    fi
}

# Main function to execute the script
main() {
    # Check if the system is Debian-based before proceeding
    check_debian_based
    setup_environment
    check_commands awk tr

    # Environment setup
    "$SCRIPTS/installer/debian_env.sh"

    # Package installation
    "$SCRIPTS/installer/debian_apt.sh"

    # System customization
    "$SCRIPTS/installer/debian_setup.sh"

    # Optional: Install desktop-related packages and customization
    #"$SCRIPTS/installer/debian_desktop_apt.sh"
    #"$SCRIPTS/installer/debian_desktop_setup.sh"
}

# Execute main function
main "$@"
