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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2025-04-27
#       Add strict error checking for folder localization file operations.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
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
        echo "[ERROR] This script is intended for macOS only." >&2
        exit 1
    fi
}

# Function to check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges for system directories." >&2
        exit 1
    fi
}

# Enable localization by creating .localized files
enable_localization() {
    echo "[INFO] Enabling folder localization..."
    for dir in "$HOME/Applications" "$HOME/Documents" "$HOME/Downloads" "$HOME/Desktop" \
               "$HOME/Public" "$HOME/Pictures" "$HOME/Music" "$HOME/Movies" "$HOME/Library"; do
        if [ -d "$dir" ]; then
            if ! touch "$dir/.localized"; then
                echo "[ERROR] Failed to create $dir/.localized." >&2
                exit 1
            fi
        fi
    done

    for dir in "/Applications" "/Applications/Utilities"; do
        if [ -d "$dir" ]; then
            if ! sudo touch "$dir/.localized"; then
                echo "[ERROR] Failed to create $dir/.localized." >&2
                exit 1
            fi
        fi
    done
    echo "[INFO] Folder localization successfully enabled."
}

# Disable localization by removing .localized files
disable_localization() {
    echo "[INFO] Disabling folder localization..."
    for dir in "$HOME/Applications" "$HOME/Documents" "$HOME/Downloads" "$HOME/Desktop" \
               "$HOME/Public" "$HOME/Pictures" "$HOME/Music" "$HOME/Movies" "$HOME/Library"; do
        if [ -f "$dir/.localized" ]; then
            if ! rm -f "$dir/.localized"; then
                echo "[ERROR] Failed to remove $dir/.localized." >&2
                exit 1
            fi
        fi
    done

    for dir in "/Applications" "/Applications/Utilities"; do
        if [ -f "$dir/.localized" ]; then
            if ! sudo rm -f "$dir/.localized"; then
                echo "[ERROR] Failed to remove $dir/.localized." >&2
                exit 1
            fi
        fi
    done
    echo "[INFO] Folder localization successfully disabled."
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
            usage
            ;;
    esac
}

# Execute main function
main "$@"
