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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
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
#  Version History:
#  v5.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v5.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
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
########################################################################

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if the system is Linux
check_system() {
    if [ "$(uname -s)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Check if required commands are available and executable
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Verify if the system is Debian-based
check_debian_based() {
    if [ ! -f /etc/os-release ]; then
        echo "[ERROR] Unable to determine the operating system." >&2
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
                    echo "[ERROR] This script is intended for Debian or Ubuntu systems only." >&2
                    exit 1
                    ;;
            esac
            ;;
    esac
}

# Verify script environment
setup_environment() {
    SCRIPTS="$HOME/scripts"
    if [ ! -d "$SCRIPTS" ]; then
        echo "[ERROR] Directory '$SCRIPTS' does not exist. Please create it or specify the correct path." >&2
        exit 1
    fi
}

# Ask for confirmation before execution
confirm_execution() {
    echo "[INFO] This script will configure your Debian-based system."
    printf "[INFO] Do you want to proceed? [y/N]: "
    read -r response < /dev/tty

    case "$response" in
        y|Y) return 0 ;;
        *) echo "[ERROR] Aborted by user."; exit 1 ;;
    esac
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
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

    echo "[INFO] All Debian initial setup completed successfully."
    return 0
}

# Execute main function
main "$@"
exit $?
