#!/bin/sh
#
# Backup and Synging Removable Disk
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
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

test -n "$1" && RSYNC_BACKUP_HOME=$1
test -n "$1" || RSYNC_BACKUP_HOME=/home/ubuntu

# df at start
df -T

# cleanup
test -x /root/bin/cleanup4mac.sh && /root/bin/cleanup4mac.sh /home/ubuntu/mnt/tc
test -x /root/bin/cleanup4mac.sh && /root/bin/cleanup4mac.sh /home/ubuntu/mnt/sdb

# svn backup
test -x /root/bin/svn_hotcopy.sh && /root/bin/svn_hotcopy.sh

# svn -> tc
test -f /root/svn_hotcopy/svn_default.tar.gz && test -d $RSYNC_BACKUP_HOME/mnt/tc/update/user2/arc/svn && cp -v /root/svn_hotcopy/svn_default.tar.gz $RSYNC_BACKUP_HOME/mnt/tc/update/user2/arc/svn/
test -f /root/svn_hotcopy/trac_default.tar.gz && test -d $RSYNC_BACKUP_HOME/mnt/tc/update/user2/arc/svn && cp -v /root/svn_hotcopy/trac_default.tar.gz $RSYNC_BACKUP_HOME/mnt/tc/update/user2/arc/svn/

# github backup
test -x /root/bin/github-arc.sh && /root/bin/github-arc.sh

# github -> tc
test -f /root/local/github.tar.gz && test -d $RSYNC_BACKUP_HOME/mnt/tc/update/user2/arc/git && cp -v /root/local/github.tar.gz $RSYNC_BACKUP_HOME/mnt/tc/update/user2/arc/git/

# tc -> sdb
echo -n "* Executing rsync tc -> sdb on "
date "+%Y/%m/%d %T"
test -d $RSYNC_BACKUP_HOME/mnt/sdb/update && test -d $RSYNC_BACKUP_HOME/mnt/tc/update && rsync -av --delete $RSYNC_BACKUP_HOME/mnt/tc/update $RSYNC_BACKUP_HOME/mnt/sdb/
test -d $RSYNC_BACKUP_HOME/mnt/sdb/user1 && test -d $RSYNC_BACKUP_HOME/mnt/tc/update/user1 && rsync -av $RSYNC_BACKUP_HOME/mnt/tc/update/user1 $RSYNC_BACKUP_HOME/mnt/sdb/
test -d $RSYNC_BACKUP_HOME/mnt/sdb/user2 && test -d $RSYNC_BACKUP_HOME/mnt/tc/update/user2 && rsync -av $RSYNC_BACKUP_HOME/mnt/tc/update/user2 $RSYNC_BACKUP_HOME/mnt/sdb/
test -d $RSYNC_BACKUP_HOME/mnt/sdb/user3/sns && test -d $RSYNC_BACKUP_HOME/mnt/tc/sns && rsync -av --delete $RSYNC_BACKUP_HOME/mnt/tc/sns $RSYNC_BACKUP_HOME/mnt/sdb/user3/
test -d $RSYNC_BACKUP_HOME/mnt/sdb/user3/doc && test -d $RSYNC_BACKUP_HOME/mnt/tc/doc && rsync -av --delete $RSYNC_BACKUP_HOME/mnt/tc/doc $RSYNC_BACKUP_HOME/mnt/sdb/user3/
test -d $RSYNC_BACKUP_HOME/mnt/sdb/user3/enterprise && test -d $RSYNC_BACKUP_HOME/mnt/tc/enterprise && rsync -av --delete $RSYNC_BACKUP_HOME/mnt/tc/enterprise $RSYNC_BACKUP_HOME/mnt/sdb/user3/
test -d $RSYNC_BACKUP_HOME/mnt/sdb/user3/personal && test -d $RSYNC_BACKUP_HOME/mnt/tc/personal && rsync -av --delete $RSYNC_BACKUP_HOME/mnt/tc/personal $RSYNC_BACKUP_HOME/mnt/sdb/user3/

# rsync func
rsync_disk2disk() {
  test -d $RSYNC_BACKUP_HOME/$2/update && echo -n "* Updating $2/update on "
  test -d $RSYNC_BACKUP_HOME/$2/update && date "+%Y/%m/%d %T"
  test -d $RSYNC_BACKUP_HOME/$2/update && test -d $RSYNC_BACKUP_HOME/mnt/tc/update && rsync -av --delete $RSYNC_BACKUP_HOME/mnt/tc/update $RSYNC_BACKUP_HOME/$2/
  echo -n "* Executing rsync $1 -> $2 on "
  date "+%Y/%m/%d %T"
  test -d $RSYNC_BACKUP_HOME/$1/user1 && test -d $RSYNC_BACKUP_HOME/$2/user1 && rsync -av --delete $RSYNC_BACKUP_HOME/$1/user1 $RSYNC_BACKUP_HOME/$2/
  test -d $RSYNC_BACKUP_HOME/$1/user2 && test -d $RSYNC_BACKUP_HOME/$2/user2 && rsync -av --delete $RSYNC_BACKUP_HOME/$1/user2 $RSYNC_BACKUP_HOME/$2/
  test -d $RSYNC_BACKUP_HOME/$1/user3 && test -d $RSYNC_BACKUP_HOME/$2/user3 && rsync -av --delete $RSYNC_BACKUP_HOME/$1/user3 $RSYNC_BACKUP_HOME/$2/
  test -d $RSYNC_BACKUP_HOME/$1/largefiles && test -d $RSYNC_BACKUP_HOME/$2/largefiles && rsync -av --delete $RSYNC_BACKUP_HOME/$1/largefiles $RSYNC_BACKUP_HOME/$2/
}

# sdb -> sdc
rsync_disk2disk mnt/sdb mnt/sdc

# sdb -> sdd
rsync_disk2disk mnt/sdb mnt/sdd

# sdb -> sde
rsync_disk2disk mnt/sdb mnt/sde

# sdb -> sdf
rsync_disk2disk mnt/sdb mnt/sdf

# df at end
df -T

