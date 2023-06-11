#!/bin/sh
#
# Backup and Syncing Removable Disk
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
# v1.25 6/11,2023
#       Disk check immediately after backup.
# v1.24 4/6,2023
#       Display and update timestamp file of disk drive.
# v1.23 4/2,2023
#       Display VeraCrypt version.
# v1.22 2/10,2023
#       Display backup directory usage statistics.
# v1.21 9/27,2016
#       Fix filesystem owner problem.
# v1.20 8/10,2016
#       Checking target host.
# v1.19 8/5,2016
#       Rename junk file cleaner.
# v1.18 8/1,2016
#       Mute tar verbose.
# v1.17 4/19,2016
#       Transfer between different ownwers.
# v1.16 5/29,2013
#       Fix device definition bugs.
# v1.15 4/18,2013
#       Directories re-construction.
# v1.14 11/14,2011
#       Backup git bare on vps.
# v1.13 9/20,2011
#       Refactoring. Move iso. Strict error code.
# v1.12 8/29,2010
#       Refactoring.
# v1.11 8/9,2010
#       Refactoring.
# v1.10 8/5,2010
#       Show return code.
#  v1.9 4/11,2010
#       Use ssh.
#  v1.8 1/4,2010
#       Show truecrypt version.
#  v1.7 12/23,2009
#       Add rsync function for portable media device.
#  v1.6 1/22,2009
#       SMART information.
#  v1.5 12/22,2008
#       Remove update area.
#  v1.4 8/20,2008
#       Add github backup.
#  v1.3 8/6,2008
#       Integrated update.
#  v1.2 5/29,2008
#       Option --delete added (user1, user2).
#  v1.1 5/12,2008
#       Repository backup added.
#  v1.0 2/28,2008
#       Stable.
########################################################################

display_and_update_timestamp() {
  test -f $T_HOME/$T_MOUNT/$T_DEVICE/timestamp && ls -l $T_HOME/$T_MOUNT/$T_DEVICE/timestamp
  touch $T_HOME/$T_MOUNT/$T_DEVICE/timestamp
}

version_info() {
  test -x /usr/bin/truecrypt && truecrypt -t --version
  test -x /usr/bin/veracrypt && veracrypt -t --version
}

smart_info() {
  test -b /dev/$B_DEVICE && smartctl -a /dev/$B_DEVICE
  test -b /dev/$T_DEVICE && smartctl -a /dev/$T_DEVICE
}

smart_check() {
  test -f $T_HOME/$T_MOUNT/$T_DEVICE/smart_longtest && test -b /dev/$T_DEVICE && smartctl -t long /dev/$T_DEVICE
  test -f $T_HOME/$T_MOUNT/$T_DEVICE/smart_longtest || test -b /dev/$T_DEVICE && smartctl -t short /dev/$T_DEVICE
}

show_capacity_of_directories() {
  test -d $B_HOME/$B_MOUNT/$B_DEVICE/largefiles && \
    du -h --max-depth=2 $B_HOME/$B_MOUNT/$B_DEVICE
}

cleanup() {
  test -x /root/bin/cleanup-junk-files.sh && \
    /root/bin/cleanup-junk-files.sh $B_HOME/$B_MOUNT/$B_DEVICE
}

svn_backup() {
  test -x /root/bin/svn_hotcopy.sh && \
    /root/bin/svn_hotcopy.sh
  test -f /root/svn_hotcopy/svn_default.tar.gz && \
    test -d $B_HOME/$B_MOUNT/$B_DEVICE/user2/arc/svn && \
    cp -v /root/svn_hotcopy/svn_default.tar.gz $B_HOME/$B_MOUNT/$B_DEVICE/user2/arc/svn/
  test -f /root/svn_hotcopy/trac_default.tar.gz && \
    test -d $B_HOME/$B_MOUNT/$B_DEVICE/user2/arc/svn && \
    cp -v /root/svn_hotcopy/trac_default.tar.gz $B_HOME/$B_MOUNT/$B_DEVICE/user2/arc/svn/
}

git_backup() {
  test -f /root/local/git.tar.gz && rm /root/local/git.tar.gz
  rsync -avz --no-o --no-g --delete $1@$2:/home/repo /root/local/
  cd /root/local
  tar czvf git.tar.gz repo/ > /dev/null
  cp -v /root/local/git.tar.gz $B_HOME/$B_MOUNT/$B_DEVICE/user2/arc/git/
  cd
}

github_backup() {
  test -x /root/bin/github-arc.sh && \
    /root/bin/github-arc.sh
  test -f /root/local/github.tar.gz && \
    test -d $B_HOME/$B_MOUNT/$B_DEVICE/user2/arc/git && \
    cp -v /root/local/github.tar.gz $B_HOME/$B_MOUNT/$B_DEVICE/user2/arc/git/
}

rsync_disk2ssh_0() {
  echo -n "* Executing rsync_disk2ssh_0 $B_DEVICE -> $T_DEVICE of $T_HOST on "
  date "+%Y/%m/%d %T"
  ping -c 1 $T_HOST > /dev/null 2>&1 && \
    test -d $B_HOME/$B_MOUNT/$B_DEVICE && \
    rsync -avz --no-o --no-g --delete -e ssh $B_HOME/$B_MOUNT/$B_DEVICE \
    $T_USER@$T_HOST:$T_HOME/$T_MOUNT/
  echo "Return code is $?"
}

rsync_disk2ssh_1() {
  echo -n "* Executing rsync_disk2ssh_1 $B_DEVICE -> $T_DEVICE of $T_HOST on "
  date "+%Y/%m/%d %T"
  ping -c 1 $T_HOST > /dev/null 2>&1 && \
    test -d $B_HOME/$B_MOUNT/$B_DEVICE/user1 && \
    rsync -avz --no-o --no-g --delete -e ssh $B_HOME/$B_MOUNT/$B_DEVICE/user1 \
    $T_USER@$T_HOST:$T_HOME/$T_MOUNT/$T_DEVICE/
  echo "Return code is $?"
  ping -c 1 $T_HOST > /dev/null 2>&1 && \
    test -d $B_HOME/$B_MOUNT/$B_DEVICE/user2 && \
    rsync -avz --no-o --no-g --delete -e ssh $B_HOME/$B_MOUNT/$B_DEVICE/user2 \
    $T_USER@$T_HOST:$T_HOME/$T_MOUNT/$T_DEVICE/
  echo "Return code is $?"
  ping -c 1 $T_HOST > /dev/null 2>&1 && \
    test -d $B_HOME/$B_MOUNT/$B_DEVICE/user3 && \
    rsync -avz --no-o --no-g --delete -e ssh $B_HOME/$B_MOUNT/$B_DEVICE/user3 \
    $T_USER@$T_HOST:$T_HOME/$T_MOUNT/$T_DEVICE/
  echo "Return code is $?"
}

rsync_disk2ssh_2() {
  echo -n "* Executing rsync_disk2ssh_2 $B_DEVICE -> $T_DEVICE of $T_HOST on "
  date "+%Y/%m/%d %T"
  ping -c 1 $T_HOST > /dev/null 2>&1 && \
    test -d $B_HOME/$B_MOUNT/$B_DEVICE/largefiles && \
    rsync -avz --no-o --no-g --delete -e ssh $B_HOME/$B_MOUNT/$B_DEVICE/largefiles \
    $T_USER@$T_HOST:$T_HOME/$T_MOUNT/$T_DEVICE/
  echo "Return code is $?"
}

rsync_disk2disk_1() {
  echo -n "* Executing rsync_disk2disk_1 $B_DEVICE -> $T_DEVICE on "
  date "+%Y/%m/%d %T"
  test -d $B_HOME/$B_MOUNT/$B_DEVICE/user1 && \
    test -d $T_HOME/$T_MOUNT/$T_DEVICE/user1 && \
    rsync -avz --no-o --no-g --delete $B_HOME/$B_MOUNT/$B_DEVICE/user1 \
    $T_HOME/$T_MOUNT/$T_DEVICE/
  echo "Return code is $?"
  test -d $B_HOME/$B_MOUNT/$B_DEVICE/user2 && \
    test -d $T_HOME/$T_MOUNT/$T_DEVICE/user2 && \
    rsync -avz --no-o --no-g --delete $B_HOME/$B_MOUNT/$B_DEVICE/user2 \
    $T_HOME/$T_MOUNT/$T_DEVICE/
  echo "Return code is $?"
  test -d $B_HOME/$B_MOUNT/$B_DEVICE/user3 && \
    test -d $T_HOME/$T_MOUNT/$T_DEVICE/user3 && \
    rsync -avz --no-o --no-g --delete $B_HOME/$B_MOUNT/$B_DEVICE/user3 \
    $T_HOME/$T_MOUNT/$T_DEVICE/
  echo "Return code is $?"
}

rsync_disk2disk_2() {
  echo -n "* Executing rsync_disk2disk_2 $B_DEVICE -> $T_DEVICE on "
  date "+%Y/%m/%d %T"
  test -d $B_HOME/$B_MOUNT/$B_DEVICE/largefiles && \
    test -d $T_HOME/$T_MOUNT/$T_DEVICE/largefiles && \
    rsync -avz --no-o --no-g --delete $B_HOME/$B_MOUNT/$B_DEVICE/largefiles \
    $T_HOME/$T_MOUNT/$T_DEVICE/
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
