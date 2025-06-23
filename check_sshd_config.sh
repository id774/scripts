#!/bin/sh

########################################################################
# check_sshd_config.sh: SSHD Configuration Check Tool
#
#  Description:
#  This script checks the SSH daemon configuration for both macOS and Linux.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
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
#  - Detects and supports both macOS and Linux environments.
#
#  This script does not require any arguments. It automatically detects
#  the operating system and checks the SSHD configuration accordingly.
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

# Function to check required commands
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
        grep -E "^(Port|PermitRootLogin|PasswordAuthentication|ChallengeResponseAuthentication|AddressFamily|AllowUsers)" "$1" | grep -v "#"
    else
        echo "[ERROR] Configuration file '$1' not found." >&2
    fi
}

# Function to handle main SSHD configuration checks
check_main_sshd() {
    sshd_config_file="/etc/ssh/sshd_config"
    check_sshd_config "$sshd_config_file"
}

# Function to handle additional SSHD configuration checks
check_additional_sshd() {
    sshd_config_file="/etc/ssh/sshd_config.d/000-sshdconfig.conf"
    if [ -f "$sshd_config_file" ]; then
        check_sshd_config "$sshd_config_file"
    fi
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac
    check_commands grep
    check_main_sshd
    check_additional_sshd
    return 0
}

# Execute main function
main "$@"
exit $?
