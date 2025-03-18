#!/bin/sh
#
########################################################################
# rsync_backup.sh: Backup and Syncing Removable Disk Script
#
#  Description:
#  This script facilitates the backup and syncing of data to removable
#  disks. It includes functionalities like checking disk health, updating
#  timestamps, performing cleanup tasks, and syncing data between disks and
#  over SSH.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.2  2023-12-23 - Refactored for POSIX compliance. Replaced Bash-specific syntax
#                     with POSIX standard commands and structures. Enhanced portability
#                     and compatibility across different UNIX-like systems.
#  v2.1  2023-12-17 - Refactored script to separate logic and operations.
#                     Operations are now defined in an external file
#                     'etc/rsync_backup.conf' for enhanced modularity and
#                     maintainability.
#                     Integrated github-arc.sh and cleanup-junk-files.sh scripts.
#  v2.0  2023-07-04 - Major version upgrade with no functional changes.
#  v1.27 2023-07-02 - Convert script to POSIX-compatible syntax. Show return
#                     code 1 if required directory does not exist.
#  [Further version history truncated for brevity]
#  2023 - Several refinements, including POSIX-compatible syntax and return code adjustments.
#         Implementation of disk health check immediately after backup.
#  2016 - Enhanced target host checking and filesystem ownership issues fixed.
#  2013 - Device definition bugs fixed and directories re-constructed.
#  2011 - Backup functionalities for git repositories and GitHub.
#  2010 - Improvements in error handling and SSH usage.
#  2009 - Addition of rsync function for portable media devices and SMART information.
#  2008 - Initial stable release, with basic backup and sync functionalities.
#  v1.0 2008-02-28 - Stable initial release.
#
#  Usage:
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
  echo "Removing junk files in $B_HOME/$B_MOUNT/$B_DEVICE..."
  echo "Removing ._* AppleDouble files..."
  find "$B_HOME/$B_MOUNT/$B_DEVICE" -name '._*' -exec rm -vf {} \;
  echo "Removing .DS_Store files..."
  find "$B_HOME/$B_MOUNT/$B_DEVICE" -name '.DS_Store' -exec rm -vf {} \;
  echo "Removing temporary Unix files ending with '.un~'..."
  find "$B_HOME/$B_MOUNT/$B_DEVICE" -name '.*.un~' -exec rm -vf {} \;
  echo "Removing __pycache__ directories..."
  find "$B_HOME/$B_MOUNT/$B_DEVICE" -type d -name '__pycache__' -exec rm -vrf {} \;
  echo "Cleanup completed."
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
  test -f /root/local/github.tar.gz && rm -f /root/local/github.tar.gz
  test -d $B_HOME/local/github && tar czvf /root/local/github.tar.gz $B_HOME/local/github > /dev/null
  du -h --max-depth=1 $B_HOME/local/github
  du -h --max-depth=1 $B_HOME/local/git

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

# Main function to execute the script
main() {
    SCRIPT_DIR="$(cd "$(dirname "$0")" >/dev/null 2>&1 && pwd)"
    CONFIG_FILE="$SCRIPT_DIR/../etc/rsync_backup.conf"

    if [ -f "$CONFIG_FILE" ]; then
        . "$CONFIG_FILE"
    else
        echo "Configuration file not found: $CONFIG_FILE">&2
        exit 99
    fi
}

# Execute main function
main "$@"
