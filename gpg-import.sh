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
#  Requirements:
#  - Debian-based Linux system with gpg, sudo, tee available.
#  - Recommended: use with APT signed-by= pointing to /usr/share/keyrings/*.gpg
#
#  Version History:
#  v2.0 2025-12-13
#       Store dearmored key under /usr/share/keyrings and add argument validation & error handling.
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

# Validate arguments
validate_args() {
    if [ "$#" -ne 2 ]; then
        echo "[ERROR] Exactly two arguments required: KEYSERVER PUBKEY" >&2
        exit 2
    fi
    return 0
}

# Import a GPG key from the specified keyserver
import_gpg_key() {
    keyserver="$1"
    pubkey="$2"

    echo "[INFO] Importing GPG key from $keyserver..."
    if ! gpg --keyserver "$keyserver" --recv-keys "$pubkey"; then
        echo "[ERROR] Failed to receive key: $pubkey from $keyserver" >&2
        exit 1
    fi

    target="/usr/share/keyrings/${pubkey}.gpg"
    echo "[INFO] Exporting dearmored key to $target ..."
    if ! sudo sh -c "gpg --export '$pubkey' | gpg --dearmor > '$target'"; then
        echo "[ERROR] Failed to write dearmored key to $target" >&2
        exit 1
    fi
    # Ensure permissions are sane (0644) respecting umask
    sudo chmod 0644 "$target" 2>/dev/null || true
    echo "[INFO] Done. Use it in sources.list as: signed-by=$target"
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    validate_args "$@"
    check_system
    check_commands gpg sudo tee
    check_sudo
    import_gpg_key "$1" "$2"
    return 0
}

# Execute main function
main "$@"
