#!/bin/bash
#
########################################################################
# Backup and Syncing Removable Disk Script
#
#  Description:
#  This script facilitates the backup and syncing of data to removable
#  disks. It includes functionalities like checking disk health, updating
#  timestamps, performing cleanup tasks, and syncing data between disks and
#  over SSH.
#
#  Author: id774
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.0  7/4,2023 - Major version upgrade with no functional changes.
#  v1.27 7/2,2023 - Convert script to POSIX-compatible syntax. Show return
#                   code 1 if required directory does not exist.
#  [Further version history truncated for brevity]
#  2023 - Several refinements, including POSIX-compatible syntax and return code adjustments.
#         Implementation of disk health check immediately after backup.
#  2016 - Enhanced target host checking and filesystem ownership issues fixed.
#  2013 - Device definition bugs fixed and directories re-constructed.
#  2011 - Backup functionalities for git repositories and GitHub.
#  2010 - Improvements in error handling and SSH usage.
#  2009 - Addition of rsync function for portable media devices and SMART information.
#  2008 - Initial stable release, with basic backup and sync functionalities.
#  v1.0 2/28,2008 - Stable initial release.
#
# Usage:
#  Run the script without any arguments. Ensure all the necessary paths
#  and variables are correctly set within the script:
#      test -x /root/bin/rsync_backup.sh && /root/bin/rsync_backup.sh>>$JOBLOG 2>&1;
#
#  The script will automatically execute operations based on the configured
#  settings and available devices.
#
########################################################################

display_and_update_timestamp() {
  if [ -f "$T_HOME/$T_MOUNT/$T_DEVICE/timestamp" ]; then
    ls -l "$T_HOME/$T_MOUNT/$T_DEVICE/timestamp"
  fi
  touch "$T_HOME/$T_MOUNT/$T_DEVICE/timestamp"
}

version_info() {
  if [ -x /usr/bin/truecrypt ]; then
    /usr/bin/truecrypt -t --version
  fi
  if [ -x /usr/bin/veracrypt ]; then
    /usr/bin/veracrypt -t --version
  fi
}

smart_info() {
  if [ -b /dev/$B_DEVICE ]; then
    smartctl -a /dev/$B_DEVICE
  fi
  if [ -b /dev/$T_DEVICE ]; then
    smartctl -a /dev/$T_DEVICE
  fi
}

smart_check() {
  if [ -b /dev/$T_DEVICE ]; then
    if [ -f "$T_HOME/$T_MOUNT/$T_DEVICE/smart_longtest" ]; then
      touch "$T_HOME/$T_MOUNT/$T_DEVICE/smart_longtest"
      smartctl -t long /dev/$T_DEVICE
    else
      touch "$T_HOME/$T_MOUNT/$T_DEVICE/smart_shorttest"
      smartctl -t short /dev/$T_DEVICE
    fi
  else
    echo "The device /dev/$T_DEVICE does not exist or is not a block device."
  fi
}

show_capacity_of_directories() {
  if [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/largefiles" ]; then
    du -h --max-depth=2 "$B_HOME/$B_MOUNT/$B_DEVICE"
  fi
}

cleanup() {
  if [ -x /root/bin/cleanup-junk-files.sh ]; then
    /root/bin/cleanup-junk-files.sh "$B_HOME/$B_MOUNT/$B_DEVICE"
  fi
}

git_backup() {
  if [ -f /root/local/git.tar.gz ]; then
    rm /root/local/git.tar.gz
  fi
  rsync -avz --no-o --no-g --delete $1@$2:/home/repo /root/local/
  cd /root/local
  tar czvf git.tar.gz repo/ > /dev/null
  cp -v /root/local/git.tar.gz "$B_HOME/$B_MOUNT/$B_DEVICE/user2/arc/git/"
  cd
}

github_backup() {
  if [ -x /root/bin/github-arc.sh ]; then
    /root/bin/github-arc.sh
  fi
  if [ -f /root/local/github.tar.gz ] && [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/user2/arc/git" ]; then
    cp -v /root/local/github.tar.gz "$B_HOME/$B_MOUNT/$B_DEVICE/user2/arc/git/"
  fi
}

rsync_disk2ssh_1() {
  echo -n "* Executing rsync_disk2ssh_1 $B_DEVICE -> $T_DEVICE of $T_HOST on "
  date "+%Y/%m/%d %T"
  if ping -c 1 $T_HOST > /dev/null 2>&1 && [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/user1" ]; then
    rsync -avz --no-o --no-g --delete -e ssh "$B_HOME/$B_MOUNT/$B_DEVICE/user1" \
    "$T_USER@$T_HOST:$T_HOME/$T_MOUNT/$T_DEVICE/"
  else
    false
  fi
  echo "Return code is $?"

  if ping -c 1 $T_HOST > /dev/null 2>&1 && [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/user2" ]; then
    rsync -avz --no-o --no-g --delete -e ssh "$B_HOME/$B_MOUNT/$B_DEVICE/user2" \
    "$T_USER@$T_HOST:$T_HOME/$T_MOUNT/$T_DEVICE/"
  else
    false
  fi
  echo "Return code is $?"

  if ping -c 1 $T_HOST > /dev/null 2>&1 && [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/user3" ]; then
    rsync -avz --no-o --no-g --delete -e ssh "$B_HOME/$B_MOUNT/$B_DEVICE/user3" \
    "$T_USER@$T_HOST:$T_HOME/$T_MOUNT/$T_DEVICE/"
  else
    false
  fi
  echo "Return code is $?"
}

rsync_disk2ssh_2() {
  echo -n "* Executing rsync_disk2ssh_2 $B_DEVICE -> $T_DEVICE of $T_HOST on "
  date "+%Y/%m/%d %T"
  if ping -c 1 $T_HOST > /dev/null 2>&1 && [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/largefiles" ]; then
    rsync -avz --no-o --no-g --delete -e ssh "$B_HOME/$B_MOUNT/$B_DEVICE/largefiles" \
    "$T_USER@$T_HOST:$T_HOME/$T_MOUNT/$T_DEVICE/"
  else
    false
  fi
  echo "Return code is $?"
}

rsync_disk2disk_1() {
  echo -n "* Executing rsync_disk2disk_1 $B_DEVICE -> $T_DEVICE on "
  date "+%Y/%m/%d %T"
  if [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/user1" ] && [ -d "$T_HOME/$T_MOUNT/$T_DEVICE/user1" ]; then
    rsync -avz --no-o --no-g --delete "$B_HOME/$B_MOUNT/$B_DEVICE/user1" \
    "$T_HOME/$T_MOUNT/$T_DEVICE/"
  else
    false
  fi
  echo "Return code is $?"

  if [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/user2" ] && [ -d "$T_HOME/$T_MOUNT/$T_DEVICE/user2" ]; then
    rsync -avz --no-o --no-g --delete "$B_HOME/$B_MOUNT/$B_DEVICE/user2" \
    "$T_HOME/$T_MOUNT/$T_DEVICE/"
  else
    false
  fi
  echo "Return code is $?"

  if [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/user3" ] && [ -d "$T_HOME/$T_MOUNT/$T_DEVICE/user3" ]; then
    rsync -avz --no-o --no-g --delete "$B_HOME/$B_MOUNT/$B_DEVICE/user3" \
    "$T_HOME/$T_MOUNT/$T_DEVICE/"
  else
    false
  fi
  echo "Return code is $?"
}

rsync_disk2disk_2() {
  echo -n "* Executing rsync_disk2disk_2 $B_DEVICE -> $T_DEVICE on "
  date "+%Y/%m/%d %T"
  if [ -d "$B_HOME/$B_MOUNT/$B_DEVICE/largefiles" ] && [ -d "$T_HOME/$T_MOUNT/$T_DEVICE/largefiles" ]; then
    rsync -avz --no-o --no-g --delete "$B_HOME/$B_MOUNT/$B_DEVICE/largefiles" \
    "$T_HOME/$T_MOUNT/$T_DEVICE/"
  else
    false
  fi
  echo "Return code is $?"
}

operation1() {
  B_HOME=/home/ubuntu
  T_HOME=/home/ubuntu
  B_MOUNT=mnt
  B_DEVICE=sdb
  T_MOUNT=mnt
  T_DEVICE=sde

  version_info
  df -T

  display_and_update_timestamp
  smart_info

  show_capacity_of_directories
  cleanup
  github_backup
  git_backup git git.id774.net

  rsync_disk2disk_1
  rsync_disk2disk_2

  df -T
  smart_check
}

operation2() {
  B_HOME=/home/ubuntu
  T_HOME=/home/ubuntu
  B_MOUNT=mnt
  B_DEVICE=sdb
  T_MOUNT=mnt

  version_info
  df -T

  display_and_update_timestamp
  smart_info

  rsync_disk2disk_1
  rsync_disk2disk_2

  df -T
  smart_check
}

operation() {
  T_HOME=/home/ubuntu
  T_MOUNT=mnt
  T_DEVICE=sde
  test -f $T_HOME/$T_MOUNT/$T_DEVICE/timestamp && operation1
  T_DEVICE=sdf
  test -f $T_HOME/$T_MOUNT/$T_DEVICE/timestamp && operation2
}

operation
