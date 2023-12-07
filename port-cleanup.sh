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

# Check if the system is macOS and 'port' command exists
if [ "$(uname)" != "Darwin" ] || ! command -v port > /dev/null; then
    echo "This script requires macOS with the 'port' command installed."
    exit 1
fi

# Script to uninstall outdated ports
for f in `port installed | grep "@" | grep -v "(active)" | sed -e "s/ //g"`;
do
  sudo port -d -f uninstall $f;
done

