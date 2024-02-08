#!/bin/sh

########################################################################
# dashcam_backup.sh: Dashcam Data Backup and Device Health Check Script
#
#  Description:
#  This script performs backups from a specified path to a target device and
#  checks the health of the device using smartctl. It is designed for dashcam
#  data management and device health monitoring. Configuration settings are
#  loaded from an external '.conf' file for easier management.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2024-02-08
#       Enhanced documentation, added configuration variable checks, improved error
#       handling, script structure, and added command checks.
#  v1.1 2023-12-23
#       Updated to load configuration settings from an external .conf file.
#       Refactored for POSIX compliance. Replaced Bash-specific syntax
#       with POSIX standard commands and structures. Enhanced portability
#       and compatibility across different UNIX-like systems.
#  v1.0 2023-11-12
#       Initial release. Features include checking for necessary files and
#       directories, performing data sync, and running device health checks.
#
#  Usage:
#  Ensure that 'dashcam_backup.conf' is properly configured with the
#  TARGET_DEVICE, BACKUP_PATH, and MOUNT_PATH variables. Run the script:
#      ./dashcam_backup.sh
#
#  The script checks for the required conditions, syncs data to the target
#  device, and performs a long test on the device.
#
#  Configuration file ('dashcam_backup.conf') requirements:
#  - TARGET_DEVICE: Identifier of the target device (e.g., sda, sdb).
#  - BACKUP_PATH: Source path for the dashcam data to be backed up.
#  - MOUNT_PATH: Mount path of the target device.
#  - TIMESTAMP_FILE: Path to a file used to record the last backup time.
#  Ensure these variables are set in 'dashcam_backup.conf'.
#
#  Notes:
#  - Requires 'smartctl' for device health checks. Ensure it's installed.
#  - Run this script with sufficient permissions to access 'smartctl' and mount points.
#
#  Error Conditions:
#  1. Backup path does not exist.
#  2. Timestamp file not found.
#  3. Configuration file not found.
#  4. Configuration variables not set.
#  5. Device health check failed.
#  6. Data sync failed.
#  7. Long test on device failed.
#  126. Required command(s) not executable.
#  127. Required command(s) not installed.
#
########################################################################

# Determine the script's directory
SCRIPT_DIR=$(dirname "$0")

# Load configuration from a .conf file
CONF_FILE="$SCRIPT_DIR/etc/dashcam_backup.conf"
if [ ! -f "$CONF_FILE" ]; then
    CONF_FILE="$SCRIPT_DIR/../etc/dashcam_backup.conf"
    if [ ! -f "$CONF_FILE" ]; then
        echo "Configuration file not found."
        exit 3
    fi
fi
. "$CONF_FILE"

# Check if necessary variables are set
if [ -z "$TARGET_DEVICE" ] || [ -z "$BACKUP_PATH" ] || [ -z "$MOUNT_PATH" ] || [ -z "$TIMESTAMP_FILE" ]; then
    echo "Error: Configuration variables not set. Check dashcam_backup.conf."
    exit 4
fi

# Check for required commands
check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again."
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions."
            exit 126
        fi
    done
}

check_commands smartctl rsync

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
    if ! sudo smartctl -a /dev/$TARGET_DEVICE; then
        echo "Device health check failed."
        exit 5
    fi
}

sync_data() {
    if ! rsync -avz --delete "$BACKUP_PATH/" "$MOUNT_PATH/"; then
        echo "Data sync failed."
        exit 6
    fi
    touch "$TIMESTAMP_FILE"
}

perform_long_test() {
    if ! sudo smartctl -t long /dev/$TARGET_DEVICE; then
        echo "Long test on device failed."
        exit 7
    fi
}

# Main script logic
check_conditions
check_device_health
sync_data
perform_long_test

