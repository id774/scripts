#!/bin/bash

########################################################################
# dashcam_backup.sh: Dashcam Data Backup and Device Health Check Script
#
#  Description:
#  This script performs backups from a specified path to a target device and
#  checks the health of the device using smartctl. It's designed for
#  dashcam data management and device health monitoring. Configuration settings
#  are now loaded from an external .conf file for easier management.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-23
#       Updated to load configuration settings from an external .conf file.
#  v1.0 2023-11-12
#       Initial release. Features include checking for necessary files and
#       directories, performing data sync, and running device health checks.
#
#  Usage:
#  Ensure that the dashcam_backup.conf file is properly configured with the
#  TARGET_DEVICE, BACKUP_PATH, and MOUNT_PATH variables. Then run the script:
#      ./dashcam_backup.sh
#
#  The script will check for the required conditions, sync data to the target
#  device, and perform a long test on the device.
#
########################################################################

# Determine the script's directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Load configuration from a .conf file
CONF_FILE="$SCRIPT_DIR/etc/dashcam_backup.conf"
if [ ! -f "$CONF_FILE" ]; then
    CONF_FILE="$SCRIPT_DIR/../etc/dashcam_backup.conf"
    if [ ! -f "$CONF_FILE" ]; then
        echo "Configuration file not found."
        exit 3
    fi
fi
source "$CONF_FILE"

# Functions
check_conditions() {
    if [ ! -d "$BACKUP_PATH" ]; then
        echo "Backup path does not exist."
        exit 1
    fi

    if [ ! -f "$TIMESTAMP_FILE" ]; then
        echo "Timestamp file not found."
        exit 2
    fi
}

check_device_health() {
    sudo smartctl -a /dev/$TARGET_DEVICE
}

sync_data() {
    rsync -avz --delete "$BACKUP_PATH" "$MOUNT_PATH/"
    touch "$TIMESTAMP_FILE"
}

perform_long_test() {
    sudo smartctl -t long /dev/$TARGET_DEVICE
}

# Main script logic
check_conditions
check_device_health
sync_data
perform_long_test

