#!/bin/sh
#
# Backup and Synging Removable Disk
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.5 1/22,2009
#       SMART information.
#  v1.4 12/22,2008
#       Remove update area.
#  v1.3 8/16,2008
#       Add directory option.
#  v1.2 8/6,2008
#       Add df.
#  v1.1 5/29,2008
#       Option --delete added (user1, user2).
#  v1.0 2/28,2008
#       Stable.
########################################################################

test -n "$1" && RSYNC_BACKUP_HOME=$1
test -n "$1" || RSYNC_BACKUP_HOME=/home/ubuntu

# df at start
df -T

# SMART information
test -b /dev/sdb && smartctl -a /dev/sdb
test -b /dev/sdc && smartctl -a /dev/sdc

# rsync func
rsync_disk2disk() {
  echo -n "* Executing rsync $1 -> $2 on "
  date "+%Y/%m/%d %T"
  test -d $RSYNC_BACKUP_HOME/$1/user1 && test -d $RSYNC_BACKUP_HOME/$2/user1 && rsync -av --delete $RSYNC_BACKUP_HOME/$1/user1 $RSYNC_BACKUP_HOME/$2/
  test -d $RSYNC_BACKUP_HOME/$1/user2 && test -d $RSYNC_BACKUP_HOME/$2/user2 && rsync -av --delete $RSYNC_BACKUP_HOME/$1/user2 $RSYNC_BACKUP_HOME/$2/
  test -d $RSYNC_BACKUP_HOME/$1/user3 && test -d $RSYNC_BACKUP_HOME/$2/user3 && rsync -av --delete $RSYNC_BACKUP_HOME/$1/user3 $RSYNC_BACKUP_HOME/$2/
  test -d $RSYNC_BACKUP_HOME/$1/largefiles && test -d $RSYNC_BACKUP_HOME/$2/largefiles && rsync -av --delete $RSYNC_BACKUP_HOME/$1/largefiles $RSYNC_BACKUP_HOME/$2/
}

# sdc -> sdb
rsync_disk2disk mnt/sdc mnt/sdb

# df at end
df -T

