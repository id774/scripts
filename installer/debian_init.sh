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
#  v5.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v5.0 2025-03-21
#       Improved system detection for Debian and Ubuntu.
#       Enhanced documentation and comments for better maintainability.
#       Ensured strict POSIX compliance.
#       Redirected error messages to stderr for better logging and debugging.
#       Added confirmation prompt before execution.
#  [Further version history truncated for brevity]
#  v0.1 2007-08-27
#       First version.
#
#  Usage:
#  Run the script directly:
#      ./debian_init.sh
#
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

# Function to ask for confirmation before execution
confirm_execution() {
    echo "This script will configure your Debian-based system."
    printf "Do you want to proceed? [y/N]: "
    read -r response < /dev/tty

    case "$response" in
        y|Y) return 0 ;;
        *) echo "Aborted by user."; exit 1 ;;
    esac
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    # Check if the system is Debian-based before proceeding
    check_system
    setup_environment
    check_commands awk tr
    check_debian_based

    # Ask for confirmation before proceeding
    confirm_execution

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
