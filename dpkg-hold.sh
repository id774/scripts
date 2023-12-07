#!/bin/sh

########################################################################
# dpkg-hold.sh: DPKG Package Management Helper Script
#
#  Description:
#  This script, dpkg-hold.sh, facilitates managing package states using the
#  dpkg package management system in Debian-based Linux distributions. It
#  allows users to update the state of a package, display its status, or
#  view usage instructions. The script includes an environment check to
#  ensure it is run on a compatible Debian-based system.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2023-12-05
#       Added environment check for Debian-based systems and refactored for clarity.
#  v1.1 2011-03-25
#       Modified to operate without requiring sudo privileges.
#  v1.0 2008-08-22
#       Initial release. Provides basic dpkg package management functionality.
#
#  Usage:
#  To set a package state:
#    ./dpkg-hold.sh package-name state
#    (Where 'state' can be 'hold', 'install', etc.)
#
#  To view the current state of a package:
#    ./dpkg-hold.sh package-name
#
#  To display usage information:
#    ./dpkg-hold.sh
#
#  Example:
#    ./dpkg-hold.sh nano install
#    ./dpkg-hold.sh nano
#
########################################################################

# Check if dpkg is installed
if ! command -v dpkg >/dev/null 2>&1; then
    echo "Error: dpkg is not installed. This script only works on Debian-based systems."
    exit 1
fi

# Function to display the current state of a package
show_package_status() {
    dpkg -l "$1"
}

# Function to set the state of a package
set_package_state() {
    echo "$1" "$2" | dpkg --set-selections
    show_package_status "$1"
}

# Main logic
if [ -n "$2" ]; then
    # If two arguments are provided, set the package state
    set_package_state "$1" "$2"
elif [ -n "$1" ]; then
    # If only one argument is provided, display the package status
    show_package_status "$1"
else
    # If no arguments are provided, display usage instructions
    echo "usage: $0 package-name [hold|install]"
fi

