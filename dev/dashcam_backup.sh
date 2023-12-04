#!/bin/bash
#
########################################################################
# Dashcam Data Backup and Device Health Check Script
#
#  Description:
#  This script performs backups from a specified path to a target device and
#  checks the health of the device using smartctl. It's designed for
#  dashcam data management and device health monitoring.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v0.1 11/12,2023
#       Initial release. Features include checking for necessary files and
#       directories, performing data sync, and running device health checks.
#
# Usage:
#  Ensure that the TARGET_DEVICE, BACKUP_PATH, and MOUNT_PATH variables are
#  set correctly. Then run the script:
#      bash dashcam_backup.sh
#
#  The script will check for the required conditions, sync data to the target
#  device, and perform a long test on the device.
#
########################################################################

# Configurable Variables
TARGET_DEVICE="sde"
BACKUP_PATH="/mnt/sdd/home/ubuntu/largefiles/dashcam"
MOUNT_PATH="$HOME/mnt/$TARGET_DEVICE"
TIMESTAMP_FILE="$MOUNT_PATH/timestamp"

# Functions
check_conditions() {
    if [ ! -f "$TIMESTAMP_FILE" ]; then
        echo "Timestamp file not found."
        exit 1
    fi

    if [ ! -d "$BACKUP_PATH" ]; then
        echo "Backup path does not exist."
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

