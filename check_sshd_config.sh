#!/bin/sh

########################################################################
# check_sshd_config.sh: SSHD Configuration Check Tool
#
#  Description:
#  This script checks the SSH daemon configuration for both macOS and Linux.
#  It defines a function to display key SSHD configuration parameters.
#  For macOS, it copies a default configuration file if it's missing, sets
#  the owner to root, and checks both the default and main configuration files.
#  For Linux, it displays key SSHD configuration parameters from the main
#  configuration file.
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
#  - Automatically copies and configures default SSHD settings on macOS.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.9 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.8 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v1.7 2025-02-19
#       Added support for detecting and displaying AddressFamily configuration.
#  v1.6 2025-01-03
#       Added support for detecting and displaying AllowUsers configuration.
#  v1.5 2024-03-04
#       Added command check functionality to ensure all required commands
#       are available before script execution.
#  v1.4 2024-03-03
#       Added a function to check SSHD configuration parameters and extended
#       macOS support to check both default and main configuration files.
#       Added ownership setting for the copied configuration file on macOS.
#  v1.3 2023-12-23
#       Refactored for POSIX compliance. Replaced Bash-specific syntax
#       with POSIX standard commands and structures. Enhanced portability
#       and compatibility across different UNIX-like systems.
#  v1.2 2023-12-06
#       Refactored for clarity and added detailed comments.
#  v1.1 2023-05-21
#       Added macOS support.
#  v1.0 2022-09-13
#       Initial release.
#
#  Usage:
#  ./check_sshd_config.sh
#  This script does not require any arguments. It automatically detects
#  the operating system and checks the SSHD configuration accordingly.
#
########################################################################

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

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

# Ensure necessary commands are available
check_commands grep sudo cp chown

# Define a function to check key SSHD configuration parameters
check_sshd_config() {
  # Display non-commented SSHD configuration parameters for Port, PermitRootLogin,
  # PasswordAuthentication, and ChallengeResponseAuthentication
  grep Port "$1" | grep -v "#"
  grep PermitRootLogin "$1" | grep -v "#"
  grep PasswordAuthentication "$1" | grep -v "#"
  grep ChallengeResponseAuthentication "$1" | grep -v "#"
  grep AddressFamily "$1" | grep -v "#"
  grep AllowUsers "$1" | grep -v "#"
}

# Detect the operating system
os=$(uname -s)

# Check and handle macOS specific configuration
if [ "$os" = "Darwin" ]; then
  # Copy the default sshd configuration file if it does not exist
  sshd_config_file="/etc/ssh/sshd_config.d/000-sshdconfig.conf"
  if [ ! -f "$sshd_config_file" ]; then
    check_sudo
    sudo cp -v "$SCRIPTS/etc/sshd_config.d/000-sshdconfig.conf" "$sshd_config_file"
    sudo chown root:wheel "$sshd_config_file"
  fi
  # Display the sshd configuration file
  check_sshd_config "$sshd_config_file"
  sshd_config_file="/etc/ssh/sshd_config"
  check_sshd_config "$sshd_config_file"
else
  # For Linux, display key configuration parameters from the sshd_config
  sshd_config_file="/etc/ssh/sshd_config"
  check_sshd_config "$sshd_config_file"
fi

exit 0

