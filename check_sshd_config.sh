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
#  - Displays non-comment lines in /etc/hosts.allow and /etc/hosts.deny.
#  - Detects and supports both macOS and Linux environments.
#
#  This script does not require any arguments. It automatically detects
#  the operating system and checks the SSHD configuration accordingly.
#
#  Version History:
#  v2.4 2026-03-02
#       Display non-comment lines in /etc/hosts.allow and /etc/hosts.deny.
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

# Define a function to check key SSHD configuration parameters
check_sshd_config() {
    if [ -f "$1" ]; then
        echo "[INFO] Showing SSHD config keys from $1: Port, PermitRootLogin, PasswordAuthentication, ChallengeResponseAuthentication, AddressFamily, AllowUsers"
        out=$(
            grep -E "^(Port|PermitRootLogin|PasswordAuthentication|ChallengeResponseAuthentication|AddressFamily|AllowUsers)" "$1" \
            | grep -v "^[[:space:]]*#"
        )
        if [ -n "$out" ]; then
            printf "%s\n" "$out"
        else
            echo "[WARN] No matching non-comment SSHD config keys found in $1" >&2
        fi
    else
        echo "[ERROR] Configuration file '$1' not found." >&2
    fi
}

# Handle main SSHD configuration checks
check_main_sshd() {
    sshd_config_file="/etc/ssh/sshd_config"
    check_sshd_config "$sshd_config_file"
}

# Handle additional SSHD configuration checks
check_additional_sshd() {
    sshd_config_file="/etc/ssh/sshd_config.d/000-sshdconfig.conf"
    if [ -f "$sshd_config_file" ]; then
        check_sshd_config "$sshd_config_file"
    fi
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
    check_commands awk grep
    check_main_sshd
    check_additional_sshd
    check_tcp_wrappers
    return 0
}

# Execute main function
main "$@"
