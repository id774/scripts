RAMDISK="$(hdid -nomount ram://1048576)"
diskutil eraseDisk HFS+ ramdisk $RAMDISK
mkdir /Volumes/ramdisk/tmp
chmod 700 /Volumes/ramdisk/tmp
mkdir /Volumes/ramdisk/Caches
chmod 700 /Volumes/ramdisk/Caches

mount -u -o noatime /
