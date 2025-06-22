#!/bin/sh

########################################################################
# unfix_compinit.sh: Temporarily revert secure settings for compinit
#
#  Description:
#  This script temporarily adjusts the ownership and permissions of
#  Homebrew-related directories to align with Homebrew's recommended
#  configuration. Specifically, it changes the ownership of the following
#  directories to the current user and their primary group, and ensures
#  that the user has write permissions:
#  - /usr/local/Homebrew
#  - /usr/local/share/zsh/
#  - /usr/local/share/zsh/site-functions
#  These changes are intended to resolve issues with `brew update`
#  and warnings from `brew doctor` about insecure directories. After
#  finishing Homebrew-related tasks, you can revert these changes using
#  `fix_compinit.sh` to restore the secure configuration.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.6 2025-04-28
#       Add error handling to ownership and permission operations.
#  v1.5 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.4 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.3 2025-03-16
#       Encapsulated all logic in functions and introduced main function.
#  v1.2 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.1 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v1.0 2025-01-17
#       Initial release. Sets Homebrew-recommended ownership and permissions.
#
#  Usage:
#      ./unfix_compinit.sh
#
#  Notes:
#  - This script requires root privileges to execute. Run it with `sudo`.
#  - This script is specifically tailored for macOS and will not function
#    on other operating systems.
#  - After using Homebrew, execute `fix_compinit.sh` to restore secure settings.
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

# Function to check if the system is macOS
check_system() {
    if [ "$(uname)" != "Darwin" ]; then
        echo "[ERROR] This script is intended for macOS only." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Check if a directory exists
check_directory() {
    if [ ! -d "$1" ]; then
        echo "[ERROR] Directory $1 does not exist." >&2
        exit 1
    fi
}

# Adjust ownership and permissions for Homebrew directories
adjust_homebrew_permissions() {
    echo "[INFO] Setting ownership and permissions for Homebrew directories on macOS..."

    # Get the current user and their primary group
    current_user=$(whoami)
    current_group=$(id -gn "$current_user")

    # Check if directories exist
    check_directory /usr/local/Homebrew
    check_directory /usr/local/share/zsh/
    check_directory /usr/local/share/zsh/site-functions

    check_sudo

    # Change ownership to the current user and their primary group
    echo "[INFO] Changing ownership to $current_user:$current_group..."
    if ! sudo chown -R "$current_user":"$current_group" /usr/local/Homebrew; then
        echo "[ERROR] Failed to change ownership of /usr/local/Homebrew." >&2
        exit 1
    fi
    if ! sudo chown -R "$current_user":"$current_group" /usr/local/share/zsh/; then
        echo "[ERROR] Failed to change ownership of /usr/local/share/zsh/." >&2
        exit 1
    fi
    if ! sudo chown -R "$current_user":"$current_group" /usr/local/share/zsh/site-functions; then
        echo "[ERROR] Failed to change ownership of /usr/local/share/zsh/site-functions." >&2
        exit 1
    fi

    # Set write permissions for the current user
    echo "[INFO] Setting write permissions for the user..."
    if ! chmod u+w /usr/local/share/zsh/; then
        echo "[ERROR] Failed to set write permission on /usr/local/share/zsh/." >&2
        exit 1
    fi
    if ! chmod u+w /usr/local/share/zsh/site-functions; then
        echo "[ERROR] Failed to set write permission on /usr/local/share/zsh/site-functions." >&2
        exit 1
    fi

    # Verify changes
    echo "[INFO] Ownership and permissions have been updated for Homebrew:"
    ls -Tld /usr/local/Homebrew
    ls -Tld /usr/local/share/zsh/
    ls -Tld /usr/local/share/zsh/site-functions
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    adjust_homebrew_permissions
}

# Execute main function
main "$@"
