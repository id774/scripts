#!/bin/sh

########################################################################
# macos_finder_settings.sh: Configure Finder and Screenshot Settings on macOS
#
#  Description:
#  This script customizes Finder and screenshot settings on macOS by:
#  - Disabling shadows in screenshots.
#  - Showing hidden files in Finder.
#  - Changing the default screenshot file name.
#  - Preventing .DS_Store files on network shares.
#  - Restarting SystemUIServer to apply changes.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script on macOS to apply Finder and screenshot settings:
#      ./macos_finder_settings.sh
#
#  Requirements:
#  - Must be executed on macOS.
#  - The script modifies Finder preferences and requires user permissions.
#
#  Version History:
#  v1.4 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.3 2025-04-27
#       Add strict error checking after applying each macOS Finder setting.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-16
#       Stable version with key Finder and screenshot settings.
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

# Apply Finder and screenshot settings
configure_finder_settings() {
    echo "[INFO] Applying macOS Finder and screenshot settings..."

    if ! defaults write com.apple.screencapture disable-shadow -boolean true; then
        echo "[ERROR] Failed to disable screenshot shadow." >&2
        exit 1
    fi

    if ! defaults write com.apple.finder AppleShowAllFiles true; then
        echo "[ERROR] Failed to show hidden files in Finder." >&2
        exit 1
    fi

    if ! defaults write com.apple.screencapture name "Screenshot"; then
        echo "[ERROR] Failed to change default screenshot file name." >&2
        exit 1
    fi

    if ! defaults write com.apple.desktopservices DSDontWriteNetworkStores true; then
        echo "[ERROR] Failed to prevent .DS_Store file creation on network shares." >&2
        exit 1
    fi

    if ! killall SystemUIServer; then
        echo "[ERROR] Failed to restart SystemUIServer." >&2
        exit 1
    fi

    echo "[INFO] macOS Finder settings applied successfully."
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    configure_finder_settings
    return 0
}

# Execute main function
main "$@"
