#!/bin/zsh
#
########################################################################
# VeraCrypt Device Mount/Unmount Script
#
#  Description:
#  This script facilitates mounting and unmounting of VeraCrypt-encrypted
#  devices. It supports both mount and unmount operations and checks for
#  device existence before attempting any operation.
#
#  Author: id774
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 11/14,2023
#       Initial release. Script includes functionality to mount and unmount
#       VeraCrypt-encrypted devices, check for device existence, and display
#       usage information.
#
# Usage:
#  To mount a device:
#      veramount [device]
#
#  To unmount a device:
#      veramount [device] unmount
#  or
#      veramount [device] umount
#
#  For help:
#      veramount -h
#
#  Note: This script requires VeraCrypt to be installed and assumes that
#  devices are located in /dev. The mount point is created in the user's
#  home directory under mnt/[device].
#
########################################################################

# Display usage information
usage() {
    echo "Usage: veramount device [unmount/umount]"
    echo "       veramount -h (for help)"
    exit 1
}

# Check if device exists
check_device_exists() {
    if [ ! -e "/dev/$1" ]; then
        echo "Error: Device /dev/$1 does not exist."
        exit 2
    fi
}

# Check arguments
if [ $# -eq 0 ] || [ "$1" = "-h" ]; then
    usage
fi

DEVICE=$1
ACTION="mount"
if [ "$2" = "unmount" ] || [ "$2" = "umount" ]; then
    ACTION="unmount"
fi
MOUNT_POINT=$HOME/mnt/$DEVICE

# Function to mount the device
mount_device() {
    check_device_exists $DEVICE
    sudo veracrypt -tc -t -k "" --protect-hidden no --fs-options utf8 /dev/$DEVICE $MOUNT_POINT
}

# Function to unmount the device
unmount_device() {
    check_device_exists $DEVICE
    sudo veracrypt -d /dev/$DEVICE
}

# Main logic
case $ACTION in
    mount)
        mount_device
        ;;
    unmount)
        unmount_device
        ;;
esac
