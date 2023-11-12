#!/bin/zsh

# Check arguments
if [ $# -lt 1 ]; then
    echo "Usage: $0 [unmount] device"
    exit 1
fi

ACTION="mount"
if [ "$1" = "unmount" ]; then
    ACTION="unmount"
    DEVICE=$2
else
    DEVICE=$1
fi
MOUNT_POINT=$HOME/mnt/$DEVICE

# Function to mount the device
mount_device() {
    sudo veracrypt -tc -t -k "" --protect-hidden no --fs-options utf8 /dev/$DEVICE $MOUNT_POINT
}

# Function to unmount the device
unmount_device() {
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
    *)
        echo "Invalid action. Use 'mount' or 'unmount'."
        exit 2
        ;;
esac

