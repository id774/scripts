#!/bin/zsh

########################################################################
# VeraCrypt Device Mount/Unmount Script
#
#  Description:
#  This script facilitates mounting and unmounting of VeraCrypt-encrypted
#  devices. It checks for VeraCrypt installation, device existence before
#  attempting any operation, and supports both mount and unmount operations.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-07
#       Added check for VeraCrypt installation.
#  v1.0 2023-11-14
#       Initial release. Script includes functionality to mount and unmount
#       VeraCrypt-encrypted devices, check for device existence, and display
#       usage information.
#
#  Usage:
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

# Check if VeraCrypt is installed
check_veracrypt_installed() {
    if ! command -v veracrypt >/dev/null 2>&1; then
        echo "Error: VeraCrypt is not installed. This script requires VeraCrypt to mount and unmount encrypted devices. Please install VeraCrypt and try again."
        exit 3
    fi
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

check_veracrypt_installed

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

