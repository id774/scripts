#!/bin/sh

########################################################################
# setup_dconf.sh: Batch Setup Script for GNOME Keybindings
#
#  Description:
#  This script configures GNOME workspace keybindings using dconf.
#  - Assigns switch-to-workspace shortcuts (Ctrl+1 to Ctrl+9).
#  - Assigns move-to-workspace shortcuts (Ctrl+Alt+1 to Ctrl+Alt+9).
#  - Ensures that Linux and a desktop environment are available before execution.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-03-16
#       Added Linux OS check.
#       Added desktop environment check.
#       Improved error handling and environment validation.
#  v1.0 2025-03-16
#       Added environment checks and improved error handling.
#       Unified keybinding functions for better maintainability.
#  v0.1 2020-02-04
#       Initial version.
#
#  Usage:
#  Run this script on a GNOME environment to configure workspace keybindings:
#      ./setup_dconf.sh
#
#  Requirements:
#  - Must be executed on a GNOME-based Linux system.
#  - Requires `dconf` installed.
#  - Requires a desktop environment.
#
########################################################################

# Function to check if the system is Linux
check_system() {
    if [ "$(uname)" != "Linux" ]; then
        echo "Error: This script is intended for Linux only." >&2
        exit 1
    fi
}

# Function to check if required commands exist
check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install it and try again." >&2
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Function to check if a desktop environment is installed
check_desktop_installed() {
    if tasksel --list-tasks | grep -q '^i.*desktop'; then
        echo "Desktop environment detected."
    else
        echo "Error: No desktop environment found. Please install a desktop environment before running this script." >&2
        exit 1
    fi
}

# Function to configure dconf keybindings
setup_dconf_keys() {
    action=$1
    key_prefix=$2
    modifier=$3

    for num in 1 2 3 4 5 6 7 8 9; do
        dconf write "/org/gnome/desktop/wm/keybindings/$key_prefix-$num" "['$modifier$num']" || {
            echo "Error: Failed to set dconf keybinding for $key_prefix-$num" >&2
            exit 1
        }
        dconf read "/org/gnome/desktop/wm/keybindings/$key_prefix-$num"
    done
}

# Main execution function
main() {
    check_system
    check_commands dconf tasksel
    check_desktop_installed
    setup_dconf_keys "Switch workspace" "switch-to-workspace" "<Primary>"
    setup_dconf_keys "Move window" "move-to-workspace" "<Primary><Alt>"
    echo "GNOME workspace keybindings successfully configured."
}

main "$@"
