#!/bin/sh

########################################################################
# ubuntu-eol-packages.sh: List Ubuntu Packages with End of Life Support
#
#  Description:
#  This script lists Ubuntu packages that have reached or are nearing
#  their end of life support. It requires dpkg and apt-cache commands.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2025-03-17
#       Encapsulated all logic into functions and introduced main function.
#  v1.1 2023-12-06
#       Added system environment check for Ubuntu.
#  v1.0 2012-05-08
#       Initial release.
#
#  Usage:
#  ./ubuntu-eol-packages.sh
#  Run this script on an Ubuntu system.
#
########################################################################

# Function to check if the system is Linux
check_system() {
    if [ "$(uname -s)" != "Linux" ]; then
        echo "Error: This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Function to check if the system is Ubuntu-based
check_ubuntu() {
    if [ ! -f /etc/lsb-release ] || [ ! -f /etc/debian_version ]; then
        echo "This script only runs on Ubuntu or Ubuntu-based systems." >&2
        exit 1
    fi
}

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Function to list Ubuntu packages with end of life support
list_eol_packages() {
    dpkg -l | cut -d ' ' -f3 | tail -n +6 | xargs apt-cache show 2>/dev/null | \
    egrep "^Pack|^Supp" | sed -e "s/^Package: /##/g" -e "s/Supported: /,/g" | tr -d '\n' | \
    sed -e "s/##/\n/g" | sort | uniq
}

# Main function to execute the script
main() {
    check_system
    check_ubuntu
    check_commands dpkg apt-cache cut tail xargs egrep sed tr sort uniq
    list_eol_packages
}

# Execute main function
main "$@"
