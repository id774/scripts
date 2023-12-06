#!/bin/sh

########################################################################
# port-upgrade: macOS Port Upgrade Script
#
#  Description:
#  This script is designed for macOS systems with the 'port' command installed.
#  It updates the MacPorts system, synchronizes port tree, and upgrades outdated ports.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-11-29
#       Added checks to confirm macOS system and presence of 'port' command.
#  v1.0 2013-04-26
#       Initial release. Basic functionality to update and upgrade MacPorts.
#
#  Usage:
#  Run the script without any arguments:
#      port-upgrade.sh
#
#  Note:
#  This script assumes that the 'port' command is installed on a macOS system.
#
########################################################################

# Check if the system is macOS and 'port' command exists
if [ "$(uname)" != "Darwin" ] || ! command -v port > /dev/null; then
    echo "This script requires macOS with the 'port' command installed."
    exit 1
fi

# Perform port update and upgrade processes
sudo port -d selfupdate
sudo port -d sync
sudo port -d -u upgrade outdated
port installed

