#!/bin/sh

########################################################################
# macos_system_folder_localizations.sh: Enable or disable folder localization on macOS
#
#  Description:
#  This script enables or disables system folder localization by creating
#  or removing `.localized` files in key system directories.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-03-16
#       Added support for enabling/disabling localization via arguments.
#       Improved error handling and environment checks.
#  v0.1 2025-02-04
#       Initial version.
#
#  Usage:
#  Enable localization:
#      ./macos_system_folder_localizations.sh enable
#  Disable localization:
#      ./macos_system_folder_localizations.sh disable
#
#  Requirements:
#  - Must be executed on macOS.
#  - Requires `sudo` for modifying system-wide directories.
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


# Function to check if the system is macOS
check_system() {
    if [ "$(uname)" != "Darwin" ]; then
        echo "Error: This script is intended for macOS only." >&2
        exit 1
    fi
}

# Function to check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges for system directories." >&2
        exit 1
    fi
}

# Enable localization by creating .localized files
enable_localization() {
    echo "Enabling folder localization..."
    for dir in "$HOME/Applications" "$HOME/Documents" "$HOME/Downloads" "$HOME/Desktop" \
               "$HOME/Public" "$HOME/Pictures" "$HOME/Music" "$HOME/Movies" "$HOME/Library"; do
        [ -d "$dir" ] && touch "$dir/.localized"
    done

    for dir in "/Applications" "/Applications/Utilities"; do
        [ -d "$dir" ] && sudo touch "$dir/.localized"
    done
    echo "Folder localization enabled."
}

# Disable localization by removing .localized files
disable_localization() {
    echo "Disabling folder localization..."
    for dir in "$HOME/Applications" "$HOME/Documents" "$HOME/Downloads" "$HOME/Desktop" \
               "$HOME/Public" "$HOME/Pictures" "$HOME/Music" "$HOME/Movies" "$HOME/Library"; do
        [ -f "$dir/.localized" ] && rm -f "$dir/.localized"
    done

    for dir in "/Applications" "/Applications/Utilities"; do
        [ -f "$dir/.localized" ] && sudo rm -f "$dir/.localized"
    done
    echo "Folder localization disabled."
}

# Main function to execute the script
main() {
    check_system
    case "$1" in
        enable)
            check_sudo
            enable_localization
            ;;
        disable)
            check_sudo
            disable_localization
            ;;
        *)
            echo "Usage: $0 {enable|disable}" >&2
            exit 0
            ;;
    esac
}

# Execute main function
main "$@"
