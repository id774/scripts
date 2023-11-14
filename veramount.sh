#!/bin/zsh

# Display usage information
usage() {
    echo "Usage: $0 device [unmount]"
    echo "       $0 -h (display this help)"
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

