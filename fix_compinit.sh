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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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
#  Usage:
#  ./fix_compinit.sh
#  This script requires root privileges to execute. Run it with `sudo`.
#
#  Notes:
#  - This script is specifically tailored for macOS and will not function
#    on other operating systems.
#  - It is recommended to verify the ownership and permissions of the
#    affected directories after execution to ensure they meet the desired
#    security standards.
#
########################################################################

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

# Fix ownership and permissions for Zsh directories
fix_permissions() {
    echo "Fixing ownership and permissions for Zsh directories on macOS..."

    # Check if directories exist before proceeding
    check_directory /usr/local/Homebrew/completions/zsh/
    check_directory /usr/local/share/zsh/

    check_sudo

    # Change ownership to root:wheel
    sudo chown -R root:wheel /usr/local/Homebrew/completions/zsh/
    sudo chown -R root:wheel /usr/local/share/zsh/

    # Set secure permissions
    sudo chmod -R 755 /usr/local/Homebrew/completions/zsh/
    sudo chmod -R 755 /usr/local/share/zsh/

    # Verify changes
    echo "Ownership and permissions have been updated:"
    ls -Tld /usr/local/Homebrew/completions/zsh/
    ls -Tld /usr/local/share/zsh/
}

# Main function to execute the script
main() {
    os=$(uname)

    # Exit if not macOS
    if [ "$os" != "Darwin" ]; then
        echo "This script is designed for macOS only." >&2
        exit 1
    fi

    fix_permissions
}

# Execute main function
main "$@"
