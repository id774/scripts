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
#  v1.8 2025-12-15
#       Resolve Homebrew prefix dynamically and process existing targets only.
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

# Check if the system is macOS
check_system() {
    if [ "$(uname -s 2>/dev/null)" != "Darwin" ]; then
        echo "[ERROR] This script is intended for macOS only." >&2
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

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Fix ownership and permissions for Zsh directories
fix_permissions() {
    echo "[INFO] Fixing ownership and permissions for Zsh directories on macOS..."

    # Detect Homebrew prefix dynamically; fallback to /usr/local
    prefix=$(brew --prefix 2>/dev/null || echo /usr/local)
    targets="
${prefix}/Homebrew/completions/zsh/
${prefix}/share/zsh/
"

    changed=0
    for d in $targets; do
        if [ -d "$d" ]; then
            echo "[INFO] Processing: $d"
            if ! sudo chown -R root:wheel "$d"; then
                echo "[ERROR] Failed to chown: $d" >&2
                exit 1
            fi
            if ! sudo chmod -R 0755 "$d"; then
                echo "[ERROR] Failed to chmod: $d" >&2
                exit 1
            fi
            ls -ld "$d"
            changed=1
        else
            echo "[WARN] Directory not found: $d" >&2
        fi
    done
    if [ "$changed" -eq 0 ]; then
        echo "[INFO] No target directories under prefix: $prefix; nothing to do."
        return 0
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands uname brew chown chmod ls
    check_sudo

    fix_permissions
    return 0
}

# Execute main function
main "$@"
