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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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
        echo "Error: This script is intended for macOS only." >&2
        exit 1
    fi
}

# Apply Finder and screenshot settings
configure_finder_settings() {
    echo "Applying macOS Finder and screenshot settings..."

    # Disable shadow in screenshots
    defaults write com.apple.screencapture disable-shadow -boolean true

    # Show hidden files in Finder
    defaults write com.apple.finder AppleShowAllFiles true

    # Change the default screenshot file name
    defaults write com.apple.screencapture name "Screenshot"

    # Prevent .DS_Store file creation on network shares
    defaults write com.apple.desktopservices DSDontWriteNetworkStores true

    # Restart SystemUIServer to apply changes
    killall SystemUIServer

    echo "macOS Finder settings applied successfully."
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    configure_finder_settings
}

# Execute main function
main "$@"
