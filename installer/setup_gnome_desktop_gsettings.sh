#!/bin/sh

########################################################################
# setup_gnome_desktop_gsettings.sh: Configure GNOME media handling settings
#
#  Description:
#  This script modifies GNOME desktop settings related to media handling.
#  It disables auto-mounting, auto-opening, and autorun for external devices.
#  The script verifies required commands before execution and provides
#  feedback on the applied changes.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-02-26
#       - Initial release with command validation and setting updates.
#
#  Usage:
#      ./setup_gnome_desktop_gsettings.sh
#
#  Features:
#  - Checks if the system is running GNU/Linux before execution.
#  - Checks if 'gsettings' command is available before execution.
#  - Modifies GNOME settings for security and usability improvements.
#  - Displays confirmation of changes after each setting update.
#
########################################################################

# Display script usage information
usage() {
    awk '
        BEGIN { in_usage = 0 }
        /^#  Usage:/ { in_usage = 1; print substr($0, 4); next }
        /^#{10}/ { if (in_usage) exit }
        in_usage && /^#/ { print substr($0, 4) }
    ' "$0"
    exit 0
}

# Function to check if the system is Linux
check_system() {
    if [ "$(uname -s)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Function to set and confirm GNOME settings
gsettings_settings() {
    SCHEMA="$1"
    KEY="$2"
    VALUE="$3"

    echo "[INFO] Setting: $SCHEMA $KEY -> $VALUE"
    if ! gsettings set "$SCHEMA" "$KEY" "$VALUE"; then
        echo "[ERROR] Failed to set $SCHEMA $KEY to $VALUE." >&2
        exit 1
    fi

    echo -n "[INFO] Confirming: $SCHEMA $KEY = "
    if ! gsettings get "$SCHEMA" "$KEY"; then
        echo "[ERROR] Failed to confirm setting $SCHEMA $KEY." >&2
        exit 1
    fi
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    check_commands gsettings

    # Apply GNOME media handling settings
    gsettings_settings org.gnome.desktop.media-handling automount false
    gsettings_settings org.gnome.desktop.media-handling automount-open false
    gsettings_settings org.gnome.desktop.media-handling autorun-never true

    echo "[INFO] GNOME media handling settings updated successfully."
}

# Execute main function
main "$@"
