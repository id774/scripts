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
#  Version History:
#  v1.4 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.3 2025-04-28
#       Add strict error checking for folder localization file operations.
#       Allow -h/--help usage on non-macOS environments by checking arguments
#       before invoking macOS-specific checks.
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
########################################################################

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if the system is macOS
check_system() {
    if [ "$(uname)" != "Darwin" ]; then
        echo "[ERROR] This script is intended for macOS only." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges
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

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version)
            usage
            ;;
        enable|disable)
            check_system
            check_sudo
            if [ "$1" = "enable" ]; then
                enable_localization
            else
                disable_localization
            fi
            ;;
        *)
            usage
            ;;
    esac
    return 0
}

# Execute main function
main "$@"
