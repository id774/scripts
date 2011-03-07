#!/bin/sh
#
########################################################################
# Daily Backup Script
#
#  Customize & Maintain: id774 <idnanashi@gmail.com>
#  Copyright (C) 2002 Takeru KOMORIYA <komoriya@paken.org>
#
#  v0.7 3/7,2011
#       Remote backup, mysqldump.
#  v0.6 10/3,2010
#       Show resources.
#  v0.5 8/5,2010
#       Show return code.
#  v0.4 1/22,2009
#       SMART information.
#  v0.3 10/15,2008
#       Add disk capacity report.
#  v0.2a 9/14,2007 (base version 0.2)
#       Log header moved.
########################################################################
# Note:
# 1. see 'man rsync' for detailed options.
# 2. Users can specify exclude directory in file 'backup_exclude'
#    like this ("/" must be followed):
#    tmp/
#    unused/
########################################################################
BACKUPDIRS="/home/debian /home/tiarra /home/plagger /var/lib/rails /var/www/html /root /etc /boot"
BACKUPTO="/home/backup"
EXPIREDAYS=10
EXECDIR=${0%/*}
EXCLUDEFILE=$EXECDIR/backup_exclude 
DATE=`date +%Y%m%d`

# Resources
uname -a
uptime
free -t
df -P -T

# SMART information
test -b /dev/sda && smartctl -a /dev/sda
test -b /dev/hda && smartctl -a /dev/hda

# delete old backup directories
echo -n "* Deleting old backup directories on "
date "+%Y/%m/%d %T"

for DIR in `ls $BACKUPTO | grep "_backup_"`
do
    BDATE=`echo $DIR | sed "s/_backup_//"`
    EXPIREDATE=`date +%Y%m%d -d "$EXPIREDAYS days ago"`
    if [ $BDATE -le $EXPIREDATE ]
    then
        echo "deleting $BACKUPTO/$DIR"
        rm -rf $BACKUPTO/$DIR
    fi
done

# rsync options
OPTS="--force --delete-excluded --delete --backup --backup-dir=$BACKUPTO/_backup_$DATE -av"
if [ -f $EXCLUDEFILE ]; then
    OPTS="$OPTS --exclude-from=$EXCLUDEFILE"
fi

# execute backup
echo -n "* Executing backup with rsync on "
date "+%Y/%m/%d %T"
for dir in $BACKUPDIRS
do
    echo "rsync $OPTS $dir $BACKUPTO"
    rsync $OPTS $dir $BACKUPTO
    echo "Return code is $?"
done

mysql_dump() {
    mysqldump --add-drop-table --add-locks --password=$2 -u $1 \
        $1 > $BACKUPTO/mysqldump/$1.sql
}

mysql_dump MYSQL_TABLE PASSWORD

mirror_to_remote() {
    if [ -d $BACKUPTO ]; then
        ping -c 1 -i 3 $1 > /dev/null 2>&1 && rsync -avz --delete -e ssh $BACKUPTO root@$1:$2
    fi
}

mirror_to_remote REMOTE_HOST REMOTE_TO

