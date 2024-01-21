#!/bin/sh

########################################################################
# port-cleanup.sh: macOS Port Cleanup Script
#
#  Description:
#  This script is used to clean up outdated ports in a macOS system using MacPorts.
#  It finds all installed ports that are not active (outdated versions) and uninstalls them.
#  This helps in maintaining a clean MacPorts environment by removing unused older versions.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2024-01-21
#       Standardized command existence check using 'check_commands' function and
#       added OS check to ensure the script is run on macOS.
#  v1.1 2023-11-30
#       Added checks to ensure the script is run on macOS with 'port' command available.
#  v1.0 2013-11-24
#       Initial release. Script to uninstall outdated ports in MacPorts.
#
#  Usage:
#  Run the script without any arguments:
#      port-cleanup.sh
#
#  Note:
#  This script is intended for use on macOS systems with MacPorts installed.
#  It requires administrative privileges to uninstall ports.
#
########################################################################

# Check if the system is macOS
if [ "$(uname)" != "Darwin" ]; then
    echo "This script requires macOS."
    exit 1
fi

# Define the check_commands function
check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again."
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions."
            exit 126
        fi
    done
}

# Check for the required commands
check_commands port

# Script to uninstall outdated ports
for f in `port installed | grep "@" | grep -v "(active)" | sed -e "s/ //g"`;
do
  sudo port -d -f uninstall $f;
done
