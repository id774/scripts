#!/bin/sh

########################################################################
# remove_tracker.sh: Script to Disable and Remove Tracker from Debian
#
#  Description:
#  This script forcibly stops, disables, and removes all Tracker-related
#  processes and packages from a Debian-based system. Tracker is a file
#  indexing and search service commonly found in GNOME environments.
#  Since its presence varies depending on the Debian version and desktop
#  environment, this script attempts to eliminate all possible instances
#  and variations of Tracker.
#
#  The script performs the following operations:
#  - Identifies and kills all running Tracker-related processes.
#  - Disables systemd user services to prevent future auto-start.
#  - Resets the Tracker database to free up storage.
#  - Individually purges all related packages for better error handling.
#  - Marks packages as "hold" to prevent reinstallation.
#  - Cleans up orphaned dependencies and cached package files.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v1.0 2025-02-04
#       Initial release.
#
#  Usage:
#  Run the script directly without any arguments:
#      ./remove_tracker.sh
#
#  To force execution even if tracker is not installed:
#      ./remove_tracker.sh -f
#
#  Requirements:
#  - Must be run on a Debian-based system.
#  - Requires sudo privileges to remove system packages.
#  - systemd should be available for service management.
#
#  Notes:
#  - This script aggressively stops and removes all Tracker components,
#    including those required for GNOME search functionality.
#  - If Tracker is needed for file search, consider disabling it instead
#    of full removal.
#  - Since package names vary by Debian version, the script attempts to
#    remove multiple variations.
#  - Services are masked to prevent automatic re-enablement in the future.
#  - The script does not prompt for confirmation when removing packages.
#  - If any package removal fails, review the output logs for troubleshooting.
#  - The `-f` option forces execution even if Tracker is not detected.
#    This can be useful if you want to remove residual packages or mask
#    systemd services regardless of installation status.
#
########################################################################

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access."
        exit 1
    fi
}

# Parse options
FORCE_REMOVE=0
while getopts "f" opt; do
    case "$opt" in
        f) FORCE_REMOVE=1 ;;
        *) exit 1 ;;
    esac
done
shift $((OPTIND -1))

# Ensure tracker is installed before proceeding, unless forced
if [ "$FORCE_REMOVE" -eq 0 ] && ! command -v tracker3 >/dev/null 2>&1 && ! command -v tracker >/dev/null 2>&1; then
    echo "Error: tracker is not installed."
    exit 1
fi

check_sudo

echo "Stopping all tracker-related processes..."

# Possible process names
for PROCESS in tracker tracker3 tracker-miner-fs tracker-extract \
               tracker-miner-apps tracker-miner-rss tracker-store \
               tracker-daemon tracker-indexer; do
    if pgrep -x "$PROCESS" >/dev/null 2>&1; then
        echo "Stopping process: $PROCESS"
        pkill -9 "$PROCESS"
    fi
done

echo "Disabling tracker services..."

# Disable systemd user services
for SERVICE in tracker.service tracker3.service tracker-miner-fs.service \
               tracker-miner-fs-3.service tracker-miner-rss.service; do
    systemctl --user stop "$SERVICE" 2>/dev/null
    systemctl --user disable "$SERVICE" 2>/dev/null
    systemctl --user mask "$SERVICE" 2>/dev/null
done

echo "Resetting tracker database..."
if command -v tracker3 >/dev/null 2>&1; then
    tracker3 reset --hard
elif command -v tracker >/dev/null 2>&1; then
    tracker reset --hard
fi

echo "Purging tracker-related packages..."

# Possible package names
for PACKAGE in tracker tracker3 tracker-miner-fs tracker-extract \
               tracker-gui tracker-miners tracker-store tracker-indexer \
               tracker-doc libtracker-control-2.0-0 libtracker-control-3-0 \
               libtracker-miner-2.0-0 libtracker-miner-3-0 \
               libtracker-sparql-2.0-0 libtracker-sparql-3-0; do
    if dpkg -l | grep -q "^ii  $PACKAGE "; then
        echo "Purging package: $PACKAGE"
        sudo apt purge -y "$PACKAGE"
    fi
done

echo "Running cleanup operations..."

# Prevent tracker from being reinstalled
for PACKAGE in tracker tracker3 tracker-miner-fs tracker-extract \
               tracker-gui tracker-miners tracker-store tracker-indexer \
               tracker-doc libtracker-control-2.0-0 libtracker-control-3-0 \
               libtracker-miner-2.0-0 libtracker-miner-3-0 \
               libtracker-sparql-2.0-0 libtracker-sparql-3-0; do
    sudo apt-mark hold "$PACKAGE"
done

# Remove orphaned packages
sudo apt autoremove -y
sudo apt autoclean -y
sudo apt clean -y

echo "Tracker has been completely removed and cleaned up."
