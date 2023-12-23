#!/bin/sh

########################################################################
# check_sshd_config.sh: SSHD Configuration Check Tool
#
#  Description:
#  This script checks the SSH daemon configuration for both macOS and Linux.
#  For macOS, it copies a default configuration file if it's missing.
#  For Linux, it displays key SSHD configuration parameters.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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

# Detect the operating system
os=$(uname -s)

# Check and handle macOS specific configuration
if [ "$os" = "Darwin" ]; then
  # Copy the default sshd configuration file if it does not exist
  sshd_config_file="/etc/ssh/sshd_config.d/000-sshdconfig.conf"
  if [ ! -f "$sshd_config_file" ]; then
    sudo cp -v "$SCRIPTS/etc/sshd_config.d/000-sshdconfig.conf" "$sshd_config_file"
  fi
  # Display the sshd configuration file
  cat "$sshd_config_file"
else
  # For Linux, display key configuration parameters from the sshd_config
  sshd_config="/etc/ssh/sshd_config"
  grep Port "$sshd_config" | grep -v "#"
  grep PermitRootLogin "$sshd_config" | grep -v "#"
  grep PasswordAuthentication "$sshd_config" | grep -v "#"
  grep ChallengeResponseAuthentication "$sshd_config" | grep -v "#"
fi

exit 0

