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
#  Version History:
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
#  Usage:
#  Run this script on macOS to apply Finder and screenshot settings:
#      ./macos_finder_settings.sh
#
#  Requirements:
#  - Must be executed on macOS.
#  - The script modifies Finder preferences and requires user permissions.
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

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    configure_finder_settings
    return 0
}

# Execute main function
main "$@"
exit $?
