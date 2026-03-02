#!/bin/sh

########################################################################
# check_sshd_config.sh: SSHD and TCP Wrappers Configuration Check Tool
#
#  Description:
#  This script checks the SSH daemon configuration and TCP Wrappers access control
#  settings for both macOS and Linux.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./check_sshd_config.sh
#
#  Features:
#  - Ensures required commands are installed and executable.
#  - Displays key SSHD configuration parameters, including:
#    - Port
#    - PermitRootLogin
#    - PasswordAuthentication
#    - ChallengeResponseAuthentication
#    - AddressFamily
#    - AllowUsers
#    - PubkeyAuthentication
#    - AuthorizedKeysFile
#  - Displays non-comment lines in /etc/hosts.allow and /etc/hosts.deny.
#  - Uses "sshd -T" to show effective SSHD settings rather than inspecting config files.
#  - Best-effort detection whether TCP Wrappers is effective for sshd (libwrap linkage).
#  - Detects and supports both macOS and Linux environments.
#
#  This script does not require any arguments. It automatically detects
#  the operating system and checks the SSHD configuration accordingly.
#
#  Version History:
#  v2.4 2026-03-02
#       Display non-comment lines in /etc/hosts.allow and /etc/hosts.deny.
#       Use "sshd -T" to show effective SSHD settings and include public key settings.
#       Detect whether TCP Wrappers is effective for sshd based on libwrap linkage.
#  v2.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v2.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v2.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v2.0 2025-03-17
#       Encapsulated all logic into functions and introduced main function.
#       Redirected error messages to stderr for better logging and debugging.
#  [Further version history truncated for brevity]
#  v1.0 2022-09-13
#       Initial release.
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

# Show effective SSHD settings using "sshd -T"
check_sshd_effective_config() {
    echo "[INFO] Showing effective SSHD settings via sshd -T"

    out=$(sudo sshd -T 2>/dev/null)
    if [ -z "$out" ]; then
        echo "[ERROR] Failed to run 'sshd -T'. Please run as root or ensure sshd is properly installed/configured." >&2
        return 1
    fi

    echo "[INFO] Extracting keys: port, permitrootlogin, passwordauthentication, challengeresponseauthentication, addressfamily, allowusers, pubkeyauthentication, authorizedkeysfile"
    printf "%s\n" "$out" | awk '
        $1 ~ /^(port|permitrootlogin|passwordauthentication|challengeresponseauthentication|addressfamily|allowusers|pubkeyauthentication|authorizedkeysfile)$/ {
            print
        }
    '
    return 0
}

# Best-effort detection whether sshd is linked with libwrap (TCP Wrappers)
check_tcp_wrappers_effective() {
    sshd_path=$(command -v sshd 2>/dev/null)
    if [ -z "$sshd_path" ]; then
        echo "[WARN] sshd not found; cannot determine whether TCP Wrappers is effective." >&2
        return 0
    fi

    os=$(uname -s 2>/dev/null)
    case "$os" in
        Linux)
            if command -v ldd >/dev/null 2>&1; then
                if ldd "$sshd_path" 2>/dev/null | grep -q "libwrap"; then
                    echo "[INFO] TCP Wrappers appears effective for sshd (libwrap linked)."
                else
                    echo "[INFO] TCP Wrappers does not appear effective for sshd (libwrap not linked)."
                fi
            else
                echo "[WARN] ldd not found; cannot determine whether TCP Wrappers is effective." >&2
            fi
            ;;
        Darwin)
            if command -v otool >/dev/null 2>&1; then
                if otool -L "$sshd_path" 2>/dev/null | grep -q "libwrap"; then
                    echo "[INFO] TCP Wrappers appears effective for sshd (libwrap linked)."
                else
                    echo "[INFO] TCP Wrappers does not appear effective for sshd (libwrap not linked)."
                fi
            else
                echo "[WARN] otool not found; cannot determine whether TCP Wrappers is effective." >&2
            fi
            ;;
        *)
            echo "[WARN] Unsupported OS '$os'; cannot determine whether TCP Wrappers is effective." >&2
            ;;
    esac
    return 0
}

# Display non-comment lines in hosts.allow and hosts.deny
check_tcp_wrappers() {
    for file in /etc/hosts.allow /etc/hosts.deny; do
        if [ -f "$file" ]; then
            echo "[INFO] Showing TCP Wrappers non-comment lines from $file"
            out=$(
                awk '
                /^[[:space:]]*#/ { next }
                /^[[:space:]]*$/ { next }
                { print }
                ' "$file"
            )
            if [ -n "$out" ]; then
                printf "%s\n" "$out"
            else
                echo "[INFO] No non-comment lines found in $file"
            fi
        fi
    done
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_commands awk grep sshd uname
    check_sudo

    check_sshd_effective_config || return $?

    check_tcp_wrappers_effective
    check_tcp_wrappers

    return 0
}

# Execute main function
main "$@"
