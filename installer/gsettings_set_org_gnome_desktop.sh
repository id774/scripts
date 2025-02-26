#!/bin/sh

########################################################################
# gsettings_set_org_gnome_desktop.sh: Configure GNOME media handling settings
#
#  Description:
#  This script modifies GNOME desktop settings related to media handling.
#  It disables auto-mounting, auto-opening, and autorun for external devices.
#  The script verifies required commands before execution and provides
#  feedback on the applied changes.
#
#  Features:
#  - Checks if the system is running GNU/Linux before execution.
#  - Checks if 'gsettings' command is available before execution.
#  - Modifies GNOME settings for security and usability improvements.
#  - Displays confirmation of changes after each setting update.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-02-26
#       - Initial release with command validation and setting updates.
#
#  Usage:
#  ./gsettings_set_org_gnome_desktop.sh
#  No arguments required.
#
########################################################################

set -e  # Exit immediately if a command exits with a non-zero status

# Check if the system is running GNU/Linux
if [ "$(uname -s)" != "Linux" ]; then
    echo "Error: This script is intended to run on GNU/Linux only." >&2
    exit 1
fi

# Function to check required commands
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

# Required command
check_commands gsettings

# Function to set and confirm GNOME settings
gsettings_settings() {
    SCHEMA="$1"
    KEY="$2"
    VALUE="$3"

    echo "Setting: $SCHEMA $KEY -> $VALUE"
    gsettings set "$SCHEMA" "$KEY" "$VALUE"
    echo -n "Confirming: $SCHEMA $KEY = "
    gsettings get "$SCHEMA" "$KEY"
}

# Apply GNOME media handling settings
gsettings_settings org.gnome.desktop.media-handling automount false
gsettings_settings org.gnome.desktop.media-handling automount-open false
gsettings_settings org.gnome.desktop.media-handling autorun-never true

echo "GNOME media handling settings updated successfully."
