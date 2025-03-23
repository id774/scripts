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

# Function to check if the system is macOS
check_system() {
    if [ "$(uname)" != "Darwin" ]; then
        echo "Error: This script is intended for macOS only." >&2
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

# Check if a directory exists
check_directory() {
    if [ ! -d "$1" ]; then
        echo "Error: Directory $1 does not exist." >&2
        exit 1
    fi
}

# Adjust ownership and permissions for Homebrew directories
adjust_homebrew_permissions() {
    echo "Setting ownership and permissions for Homebrew directories on macOS..."

    # Get the current user and their primary group
    current_user=$(whoami)
    current_group=$(id -gn "$current_user")

    # Check if directories exist
    check_directory /usr/local/Homebrew
    check_directory /usr/local/share/zsh/
    check_directory /usr/local/share/zsh/site-functions

    check_sudo

    # Change ownership to the current user and their primary group
    sudo chown -R "$current_user":"$current_group" /usr/local/Homebrew
    sudo chown -R "$current_user":"$current_group" /usr/local/share/zsh/
    sudo chown -R "$current_user":"$current_group" /usr/local/share/zsh/site-functions

    # Set write permissions for the current user
    chmod u+w /usr/local/share/zsh/
    chmod u+w /usr/local/share/zsh/site-functions

    # Verify changes
    echo "Ownership and permissions have been updated for Homebrew:"
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
