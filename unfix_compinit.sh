#!/bin/sh

########################################################################
# unfix_compinit.sh: Temporarily revert secure settings for compinit
#
#  Description:
#  This script temporarily adjusts the ownership and permissions of
#  Homebrew-related directories to align with Homebrew's recommended
#  configuration. The target directories are resolved from the running
#  Homebrew installation:
#  - the Homebrew repository (`brew --repository`)
#  - `share/zsh` under the Homebrew prefix (`brew --prefix`)
#  - `share/zsh/site-functions` under the Homebrew prefix
#  Each existing target is changed to the ownership of the invoking regular
#  non-root Homebrew user and that user's primary group, and the user is
#  given write permission on it. These changes are intended to resolve
#  issues with `brew update` and warnings from `brew doctor` about insecure
#  directories. After finishing Homebrew-related tasks, you can revert these
#  changes using `fix_compinit.sh` to restore the secure configuration.
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./unfix_compinit.sh
#
#  Requirements:
#  - macOS with Homebrew installed and available in PATH.
#  - Run as a regular non-root user with sudo privileges.
#
#  Notes:
#  - Do not invoke the whole script as root or through sudo; privileged
#    ownership and permission changes are performed internally with sudo.
#  - This script is specifically tailored for macOS and will not function
#    on other operating systems.
#  - After using Homebrew, execute `fix_compinit.sh` to restore secure settings.
#
#  Exit Status:
#  0: Success.
#  1: Unsupported system, root execution, user/path resolution failure,
#     sudo failure, or ownership update failure.
#  126: Required command exists but is not executable.
#  127: Required command is not found.
#
#  Version History:
#  v2.0 2026-09-07
#       Resolve Homebrew paths and preserve non-root user ownership.
#  v1.9 2026-07-11
#       Replace the awk {n,} interval expression in usage() with a portable
#       equivalent, since mawk on some systems matches it incorrectly.
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
#       Initial release.
#
########################################################################

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

# Check if the system is macOS
check_system() {
    check_commands uname
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
    check_commands sudo
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Resolve the invoking regular user and primary group, refusing root
resolve_current_user() {
    current_uid=$(id -u 2>/dev/null)
    if [ $? -ne 0 ] || [ -z "$current_uid" ]; then
        echo "[ERROR] Failed to determine the invoking user." >&2
        exit 1
    fi
    if [ "$current_uid" -eq 0 ]; then
        echo "[ERROR] Run this script as a regular user with sudo access, not as root." >&2
        exit 1
    fi

    current_user=$(id -un 2>/dev/null)
    if [ $? -ne 0 ] || [ -z "$current_user" ]; then
        echo "[ERROR] Failed to determine the invoking user." >&2
        exit 1
    fi

    current_group=$(id -gn "$current_user" 2>/dev/null)
    if [ $? -ne 0 ] || [ -z "$current_group" ]; then
        echo "[ERROR] Failed to determine the primary group for '$current_user'." >&2
        exit 1
    fi
}

# Resolve and validate the Homebrew prefix and repository paths
resolve_homebrew_paths() {
    homebrew_prefix=$(brew --prefix 2>/dev/null)
    if [ $? -ne 0 ] || [ -z "$homebrew_prefix" ]; then
        echo "[ERROR] Failed to resolve Homebrew prefix." >&2
        exit 1
    fi

    homebrew_repository=$(brew --repository 2>/dev/null)
    if [ $? -ne 0 ] || [ -z "$homebrew_repository" ]; then
        echo "[ERROR] Failed to resolve Homebrew repository." >&2
        exit 1
    fi

    case "$homebrew_prefix" in
        /?*) ;;
        *)
            echo "[ERROR] Unsafe Homebrew prefix: $homebrew_prefix" >&2
            exit 1
            ;;
    esac

    case "$homebrew_repository" in
        /|/usr|/usr/local|/opt)
            echo "[ERROR] Unsafe Homebrew repository: $homebrew_repository" >&2
            exit 1
            ;;
        "$homebrew_prefix"|"$homebrew_prefix"/?*) ;;
        *)
            echo "[ERROR] Unsafe Homebrew repository: $homebrew_repository" >&2
            exit 1
            ;;
    esac
}

# Adjust ownership and permissions for Homebrew directories
adjust_homebrew_permissions() {
    echo "[INFO] Setting ownership and permissions for Homebrew directories on macOS..."

    resolve_homebrew_paths
    targets="
${homebrew_repository}
${homebrew_prefix}/share/zsh/
${homebrew_prefix}/share/zsh/site-functions
"

    changed=0
    for d in $targets; do
        if [ -d "$d" ]; then
            echo "[INFO] Processing: $d"
            if ! sudo chown -R "$current_user":"$current_group" "$d"; then
                echo "[ERROR] Failed to change ownership of $d." >&2
                exit 1
            fi
            if ! sudo chmod u+w "$d" 2>/dev/null; then
                echo "[WARN] Could not set write permission on $d (may be fine for protected paths)." >&2
            fi
            ls -ld "$d"
            changed=1
        else
            echo "[WARN] Directory not found: $d" >&2
        fi
    done
    if [ "$changed" -eq 0 ]; then
        echo "[INFO] No target directories under prefix: $homebrew_prefix; nothing to do."
        return 0
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands brew chown chmod ls id
    resolve_current_user
    check_sudo

    adjust_homebrew_permissions
    return 0
}

# Execute main function
main "$@"
