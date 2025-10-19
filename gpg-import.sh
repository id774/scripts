#!/bin/sh

########################################################################
# gpg-import.sh: GPG Key Import Script for APT
#
#  Description:
#  This script imports a GPG public key from a specified keyserver and
#  adds it to the APT keyring. It's useful for adding external repository
#  keys securely. Only works on Debian-based systems.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./gpg-import.sh KEYSERVER PUBKEY
#
#  Version History:
#  v1.9 2025-08-04
#       Replace deprecated 'apt-key add -' with trusted.gpg.d key file export.
#  v1.8 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.7 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.6 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.5 2025-03-16
#       Encapsulated all logic in functions and introduced main function.
#  v1.4 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.3 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v1.2 2024-01-18
#       Standardized command existence checks using a common function.
#  v1.1 2023-12-05
#       Added environment check for Debian-based systems.
#       Refactored for improved readability and added usage information.
#  v1.0 2008-08-22
#       Initial release. Imports GPG keys for APT from a keyserver.
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
    if [ "$(uname -s 2>/dev/null)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
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

# Import a GPG key from the specified keyserver
import_gpg_key() {
    keyserver="$1"
    pubkey="$2"

    echo "[INFO] Importing GPG key from $keyserver..."
    gpg --keyserver "$keyserver" --recv-keys "$pubkey"

    echo "[INFO] Exporting and adding the GPG key to APT keyring..."
    sudo gpg --armor --export "$pubkey" | sudo tee /etc/apt/trusted.gpg.d/"$pubkey".asc > /dev/null
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    # Check if both arguments are provided
    if [ -n "$2" ]; then
        check_system
        check_commands gpg sudo
        check_sudo
        import_gpg_key "$1" "$2"
    else
        usage
    fi
    return 0
}

# Execute main function
main "$@"
