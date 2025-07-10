#!/bin/sh

########################################################################
# fix_compinit.sh: Fix ownership and permissions for Zsh directories
#
#  Description:
#  This script is designed to address the "compinit: insecure directories"
#  warning that may occur when initializing Zsh completions. It adjusts
#  the ownership and permissions of Zsh-related directories used by Homebrew
#  to meet the security requirements of `compinit`. Specifically, the script
#  changes the ownership of the following directories to `root:wheel`:
#  - /usr/local/Homebrew/completions/zsh/
#  - /usr/local/share/zsh/
#  Additionally, it modifies the permissions of these directories to
#  prevent insecure write access, which can cause warnings during Zsh
#  initialization. The script is designed to run exclusively on macOS
#  (Darwin) and will exit with an error code on other operating systems.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./fix_compinit.sh
#
#  Notes:
#  - This script requires root privileges to execute. Run it with `sudo`.
#  - This script is specifically tailored for macOS and will not function
#    on other operating systems.
#  - It is recommended to verify the ownership and permissions of the
#    affected directories after execution to ensure they meet the desired
#    security standards.
#
#  Version History:
#  v1.7 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
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
#       Initial release. Ensures secure ownership and permissions for
#       Zsh-related directories used by Homebrew.
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

# Fix ownership and permissions for Zsh directories
fix_permissions() {
    echo "[INFO] Fixing ownership and permissions for Zsh directories on macOS..."

    # Check if directories exist before proceeding
    check_directory /usr/local/Homebrew/completions/zsh/
    check_directory /usr/local/share/zsh/

    check_sudo

    # Change ownership to root:wheel
    echo "[INFO] Changing ownership to root:wheel..."
    if ! sudo chown -R root:wheel /usr/local/Homebrew/completions/zsh/; then
        echo "[ERROR] Failed to change ownership of /usr/local/Homebrew/completions/zsh/." >&2
        exit 1
    fi
    if ! sudo chown -R root:wheel /usr/local/share/zsh/; then
        echo "[ERROR] Failed to change ownership of /usr/local/share/zsh/." >&2
        exit 1
    fi

    # Set secure permissions
    echo "[INFO] Setting directory permissions to 755..."
    if ! sudo chmod -R 755 /usr/local/Homebrew/completions/zsh/; then
        echo "[ERROR] Failed to set permissions on /usr/local/Homebrew/completions/zsh/." >&2
        exit 1
    fi
    if ! sudo chmod -R 755 /usr/local/share/zsh/; then
        echo "[ERROR] Failed to set permissions on /usr/local/share/zsh/." >&2
        exit 1
    fi

    # Verify changes
    echo "[INFO] Ownership and permissions have been updated:"
    ls -Tld /usr/local/Homebrew/completions/zsh/
    ls -Tld /usr/local/share/zsh/
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    fix_permissions
    return 0
}

# Execute main function
main "$@"
exit $?
