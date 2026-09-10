#!/bin/sh

########################################################################
# gpg-import.sh: GPG Key Import Script for APT
#
#  Description:
#  This script imports a GPG public key from a specified keyserver into the
#  invoking user's GPG keyring, then exports and dearmors it from that same
#  keyring. The resulting binary keyring is installed to
#  /usr/share/keyrings/PUBKEY.gpg with mode 0644, for use with APT's
#  signed-by= option. Only works on Debian-based Linux systems.
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./gpg-import.sh KEYSERVER PUBKEY
#
#  Requirements:
#  - Debian-based Linux system.
#  - Run as a regular non-root user with sudo privileges.
#
#  Exit Status:
#  0: Success.
#  1: System, privilege, network, export, conversion, or installation failure.
#  2: Invalid arguments.
#  126: Required command exists but is not executable.
#  127: Required command is not found.
#
#  Notes:
#  - Key receive, export, and dearmor run in the invoking user's GPG context.
#  - Use the generated /usr/share/keyrings/*.gpg file with APT signed-by=.
#
#  Version History:
#  v2.2 2026-09-07
#       Export from the user keyring and reject unsafe keyring paths.
#  v2.1 2026-07-11
#       Replace the awk {n,} interval expression in usage() with a portable
#       equivalent, since mawk on some systems matches it incorrectly.
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
#       Initial release.
#
########################################################################

# Temporary files for the exported and dearmored key, populated by import_gpg_key
export_file=""
dearmored_file=""

# Display full script header information extracted from the top comment block
usage() {
    check_commands awk
    awk '
        BEGIN { in_header = 0 }
        /^#+$/ && length($0) >= 10 { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if the system is Linux
check_system() {
    check_commands uname
    if [ "$(uname -s 2>/dev/null)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Check that the script is invoked by a regular non-root user
check_user() {
    invoking_uid=$(id -u 2>/dev/null)
    if [ -z "$invoking_uid" ]; then
        echo "[ERROR] Failed to determine the invoking user." >&2
        exit 1
    fi
    if [ "$invoking_uid" -eq 0 ]; then
        echo "[ERROR] Run this script as a regular user with sudo access, not as root." >&2
        exit 1
    fi
    return 0
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    check_commands sudo
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

    case "$2" in
        '' | */*)
            echo "[ERROR] PUBKEY must be a non-empty key identifier without '/' characters." >&2
            exit 2
            ;;
    esac

    return 0
}

# Remove staged temporary key files, guarding against empty or missing paths
cleanup_temp_files() {
    if [ -n "$export_file" ] && [ -f "$export_file" ]; then
        rm -f "$export_file"
    fi
    if [ -n "$dearmored_file" ] && [ -f "$dearmored_file" ]; then
        rm -f "$dearmored_file"
    fi
}

# Import a GPG key from the specified keyserver
import_gpg_key() {
    keyserver="$1"
    pubkey="$2"
    target="/usr/share/keyrings/${pubkey}.gpg"

    echo "[INFO] Importing GPG key from $keyserver..."
    if ! gpg --keyserver "$keyserver" --recv-keys "$pubkey"; then
        echo "[ERROR] Failed to receive key: $pubkey from $keyserver" >&2
        exit 1
    fi

    if ! export_file=$(mktemp); then
        echo "[ERROR] Failed to create temporary file for GPG key export." >&2
        exit 1
    fi
    trap cleanup_temp_files EXIT

    if ! dearmored_file=$(mktemp); then
        echo "[ERROR] Failed to create temporary file for dearmored GPG key." >&2
        exit 1
    fi

    if ! gpg --export "$pubkey" > "$export_file"; then
        echo "[ERROR] Failed to export key: $pubkey" >&2
        exit 1
    fi

    if ! gpg --dearmor < "$export_file" > "$dearmored_file"; then
        echo "[ERROR] Failed to dearmor key: $pubkey" >&2
        exit 1
    fi

    echo "[INFO] Exporting dearmored key to $target ..."
    if ! sudo install -m 0644 "$dearmored_file" "$target"; then
        echo "[ERROR] Failed to write dearmored key to $target" >&2
        exit 1
    fi
    echo "[INFO] Done. Use it in sources.list as: signed-by=$target"
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    validate_args "$@"
    check_system
    check_commands gpg id mktemp rm install
    check_user
    check_sudo
    import_gpg_key "$1" "$2"
    return 0
}

# Execute main function
main "$@"
