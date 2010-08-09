#!/bin/sh
#
# Backup and Synging Removable Disk
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
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

RSYNC_BACKUP_HOME=/home/debian
RSYNC_TARGET_HOME=/home/debian
RSYNC_BACKUP_MOUNTPOUNT=mnt
RSYNC_BACKUP_DEVICE=sdb
RSYNC_TARGET_MOUNTPOUNT=mnt
RSYNC_TARGET_DEVICE=sdb

truecrypt -t --version

# df at start
df -T

# SMART information
test -b /dev/sdb && smartctl -a /dev/sdb
test -b /dev/sdc && smartctl -a /dev/sdc
test -b /dev/sdd && smartctl -a /dev/sdd
test -b /dev/sde && smartctl -a /dev/sde
test -b /dev/sdf && smartctl -a /dev/sdf

cleanup() {
  test -x /root/bin/cleanup4mac.sh && /root/bin/cleanup4mac.sh $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE
}

svn_backup() {
  test -x /root/bin/svn_hotcopy.sh && /root/bin/svn_hotcopy.sh
  test -f /root/svn_hotcopy/svn_default.tar.gz && test -d $RSYNC_BACKUP_HOME/mnt/sdb/user2/arc/svn && cp -v /root/svn_hotcopy/svn_default.tar.gz $RSYNC_BACKUP_HOME/mnt/sdb/user2/arc/svn/
  test -f /root/svn_hotcopy/trac_default.tar.gz && test -d $RSYNC_BACKUP_HOME/mnt/sdb/user2/arc/svn && cp -v /root/svn_hotcopy/trac_default.tar.gz $RSYNC_BACKUP_HOME/mnt/sdb/user2/arc/svn/
}

github_backup() {
  test -x /root/bin/github-arc.sh && /root/bin/github-arc.sh
  test -f /root/local/github.tar.gz && test -d $RSYNC_BACKUP_HOME/mnt/sdb/user2/arc/git && cp -v /root/local/github.tar.gz $RSYNC_BACKUP_HOME/mnt/sdb/user2/arc/git/
}

rsync_disk2ssh_0() {
  echo -n "* Executing rsync_disk2ssh_0 $RSYNC_BACKUP_DEVICE -> $RSYNC_TARGET_DEVICE of $TARGET_HOST on "
  date "+%Y/%m/%d %T"
  test -d $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE && rsync -avz --delete -e ssh $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE $TARGET_USER@$TARGET_HOST:$RSYNC_TARGET_HOME/$RSYNC_TARGET_MOUNTPOUNT/
  echo "Return code is $?"
}

rsync_disk2ssh_1() {
  echo -n "* Executing rsync_disk2ssh_1 $RSYNC_BACKUP_DEVICE -> $RSYNC_TARGET_DEVICE of $TARGET_HOST on "
  date "+%Y/%m/%d %T"
  test -d $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/user1 && rsync -avz --delete -e ssh $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/user1 $TARGET_USER@$TARGET_HOST:$RSYNC_TARGET_HOME/$RSYNC_TARGET_MOUNTPOUNT/$RSYNC_TARGET_DEVICE/
  test -d $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/user2 && rsync -avz --delete -e ssh $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/user2 $TARGET_USER@$TARGET_HOST:$RSYNC_TARGET_HOME/$RSYNC_TARGET_MOUNTPOUNT/$RSYNC_TARGET_DEVICE/
  test -d $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/user3 && rsync -avz --delete -e ssh $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/user3 $TARGET_USER@$TARGET_HOST:$RSYNC_TARGET_HOME/$RSYNC_TARGET_MOUNTPOUNT/$RSYNC_TARGET_DEVICE/
  test -d $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/largefiles/iso && rsync -avz --delete -e ssh $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/largefiles/iso $TARGET_USER@$TARGET_HOST:$RSYNC_TARGET_HOME/$RSYNC_TARGET_MOUNTPOUNT/$RSYNC_TARGET_DEVICE/largefiles/
  test -d $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/largefiles/VMwareImages && rsync -avz --delete -e ssh $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/largefiles/VMwareImages $TARGET_USER@$TARGET_HOST:$RSYNC_TARGET_HOME/$RSYNC_TARGET_MOUNTPOUNT/$RSYNC_TARGET_DEVICE/largefiles/
  echo "Return code is $?"
}

rsync_disk2ssh_2() {
  echo -n "* Executing rsync_disk2ssh_2 $RSYNC_BACKUP_DEVICE -> $RSYNC_TARGET_DEVICE of $TARGET_HOST on "
  date "+%Y/%m/%d %T"
  test -d $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/largefiles/Movies && rsync -avz --delete -e ssh $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/largefiles/Movies $TARGET_USER@$TARGET_HOST:$RSYNC_TARGET_HOME/$RSYNC_TARGET_MOUNTPOUNT/$RSYNC_TARGET_DEVICE/largefiles/
  echo "Return code is $?"
}

rsync_disk2disk_1() {
  echo -n "* Executing rsync_disk2disk_1 $RSYNC_BACKUP_DEVICE -> $RSYNC_TARGET_DEVICE on "
  date "+%Y/%m/%d %T"
  test -d $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/user1 && test -d $RSYNC_BACKUP_HOME/$RSYNC_TARGET_MOUNTPOUNT/$RSYNC_TARGET_DEVICE/user1 && rsync -av --delete $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/user1 $RSYNC_BACKUP_HOME/$RSYNC_TARGET_MOUNTPOUNT/$RSYNC_TARGET_DEVICE/
  test -d $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/user2 && test -d $RSYNC_BACKUP_HOME/$RSYNC_TARGET_MOUNTPOUNT/$RSYNC_TARGET_DEVICE/user2 && rsync -av --delete $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/user2 $RSYNC_BACKUP_HOME/$RSYNC_TARGET_MOUNTPOUNT/$RSYNC_TARGET_DEVICE/
  test -d $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/user3 && test -d $RSYNC_BACKUP_HOME/$RSYNC_TARGET_MOUNTPOUNT/$RSYNC_TARGET_DEVICE/user3 && rsync -av --delete $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/user3 $RSYNC_BACKUP_HOME/$RSYNC_TARGET_MOUNTPOUNT/$RSYNC_TARGET_DEVICE/
  test -d $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/largefiles/iso && test -d $RSYNC_BACKUP_HOME/$RSYNC_TARGET_MOUNTPOUNT/$RSYNC_TARGET_DEVICE/largefiles && rsync -av --delete $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/largefiles/iso $RSYNC_BACKUP_HOME/$RSYNC_TARGET_MOUNTPOUNT/$RSYNC_TARGET_DEVICE/largefiles/
  test -d $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/largefiles/VMwareImages && test -d $RSYNC_BACKUP_HOME/$RSYNC_TARGET_MOUNTPOUNT/$RSYNC_TARGET_DEVICE/largefiles && rsync -av --delete $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/largefiles/VMwareImages $RSYNC_BACKUP_HOME/$RSYNC_TARGET_MOUNTPOUNT/$RSYNC_TARGET_DEVICE/largefiles/
  echo "Return code is $?"
}

rsync_disk2disk_2() {
  echo -n "* Executing rsync_disk2disk_2 $RSYNC_BACKUP_DEVICE -> $RSYNC_TARGET_DEVICE on "
  date "+%Y/%m/%d %T"
  test -d $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/largefiles/Movies && test -d $RSYNC_BACKUP_HOME/$RSYNC_TARGET_MOUNTPOUNT/$RSYNC_TARGET_DEVICE/largefiles && rsync -av --delete $RSYNC_BACKUP_HOME/$RSYNC_BACKUP_MOUNTPOUNT/$RSYNC_BACKUP_DEVICE/largefiles/Movies $RSYNC_BACKUP_HOME/$RSYNC_TARGET_MOUNTPOUNT/$RSYNC_TARGET_DEVICE/largefiles/
  echo "Return code is $?"
}

cleanup
svn_backup
github_backup

#RSYNC_TARGET_DEVICE=sdc
#rsync_disk2disk_1
#rsync_disk2disk_2

#RSYNC_TARGET_DEVICE=sdb
#TARGET_USER=
#TARGET_HOST=
#rsync_disk2ssh_0

# df at end
df -T

