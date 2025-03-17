#!/bin/sh

########################################################################
# check_sshd_config.sh: SSHD Configuration Check Tool
#
#  Description:
#  This script checks the SSH daemon configuration for both macOS and Linux.
#
#  Features:
#  - Ensures required commands are installed and executable.
#  - Displays key SSHD configuration parameters, including:
#    - Port
#    - PermitRootLogin
#    - PasswordAuthentication
#    - ChallengeResponseAuthentication
#    - AddressFamily (added in v1.7)
#    - AllowUsers (newly added in v1.6)
#  - Detects and supports both macOS and Linux environments.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.0 2025-03-17
#       Encapsulated all logic into functions and introduced main function.
#       Redirected error messages to stderr for better logging and debugging.
#  [Further version history truncated for brevity]
#  v1.0 2022-09-13
#       Initial release.
#
#  Usage:
#  ./check_sshd_config.sh
#  This script does not require any arguments. It automatically detects
#  the operating system and checks the SSHD configuration accordingly.
#
########################################################################

# Check for required commands
check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Define a function to check key SSHD configuration parameters
check_sshd_config() {
    if [ -f "$1" ]; then
        grep -E "^(Port|PermitRootLogin|PasswordAuthentication|ChallengeResponseAuthentication|AddressFamily|AllowUsers)" "$1" | grep -v "#"
    else
        echo "Error: Configuration file '$1' not found." >&2
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

# Main function
main() {
    # Ensure necessary commands are available
    check_commands grep

    check_main_sshd
    check_additional_sshd
}

# Execute main function
main "$@"
