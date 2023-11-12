#!/bin/bash

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

