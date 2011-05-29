RAMDISK="$(hdid -nomount ram://1048576)"
diskutil eraseDisk HFS+ ramdisk $RAMDISK
mkdir /Volumes/ramdisk/tmp
chmod 770 /Volumes/ramdisk/tmp
chown root:wheel /Volumes/ramdisk/tmp
mkdir /Volumes/ramdisk/Caches
chmod 770 /Volumes/ramdisk/Caches
chown root:staff /Volumes/ramdisk/Caches

mount -u -o noatime /
