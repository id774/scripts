#!/bin/sh

########################################################################
# unfix_compinit.sh: Temporarily revert secure settings for compinit
#
#  Description:
#  This script temporarily adjusts the ownership and permissions of
#  Homebrew-related directories to align with Homebrew's recommended
#  configuration. Specifically, it changes the ownership of the following
#  directories to the current user and their primary group, and ensures
#  that the user has write permissions:
#  - /usr/local/Homebrew
#  - /usr/local/share/zsh/
#  - /usr/local/share/zsh/site-functions
#  These changes are intended to resolve issues with `brew update`
#  and warnings from `brew doctor` about insecure directories. After
#  finishing Homebrew-related tasks, you can revert these changes using
#  `fix_compinit.sh` to restore the secure configuration.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-01-17
#       Initial release. Sets Homebrew-recommended ownership and permissions.
#
#  Usage:
#  ./unfix_compinit.sh
#  This script requires root privileges to execute. Run it with `sudo`.
#
#  Notes:
#  - This script is specifically tailored for macOS and will not function
#    on other operating systems.
#  - After using Homebrew, execute `fix_compinit.sh` to restore secure settings.
#
########################################################################

# Check if a directory exists
check_directory() {
  if [ ! -d "$1" ]; then
    echo "Error: Directory $1 does not exist." >&2
    exit 1
  fi
}

# Determine the operating system
os=$(uname)

if [ "$os" = "Darwin" ]; then
  echo "Setting ownership and permissions for Homebrew directories on macOS..."

  # Get the current user and their primary group
  current_user=$(whoami)
  current_group=$(id -gn "$current_user")

  # Check if directories exist
  check_directory /usr/local/Homebrew
  check_directory /usr/local/share/zsh/
  check_directory /usr/local/share/zsh/site-functions

  # Change ownership to the current user and their primary group
  sudo chown -R "$current_user":"$current_group" /usr/local/Homebrew
  sudo chown -R "$current_user":"$current_group" /usr/local/share/zsh/
  sudo chown -R "$current_user":"$current_group" /usr/local/share/zsh/site-functions

  # Set write permissions for the current user
  chmod u+w /usr/local/share/zsh/
  chmod u+w /usr/local/share/zsh/site-functions

  # Verify changes
  echo "Ownership and permissions have been updated for Homebrew:"
  ls -ld /usr/local/Homebrew
  ls -ld /usr/local/share/zsh/
  ls -ld /usr/local/share/zsh/site-functions

else
  echo "This script is designed for macOS only." >&2
  exit 1
fi

exit 0
