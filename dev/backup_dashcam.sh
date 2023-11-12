#!/bin/bash

T_DEVICE=sde
B_PATH=/mnt/sdd/home/ubuntu/largefiles/dashcam
test -f $HOME/mnt/$T_DEVICE/timestamp || exit 1
test -d $B_PATH || exit 2
sudo smartctl -a /dev/$T_DEVICE
rsync -avz --delete $B_PATH $HOME/mnt/$T_DEVICE/
touch $HOME/mnt/$T_DEVICE/timestamp
sudo smartctl -t long /dev/$T_DEVICE
