#!/bin/sh
#
########################################################################
# Daily Backup Script
#
#  Customize & Maintain: id774 <idnanashi@gmail.com>
#  Copyright (C) 2002 Takeru KOMORIYA <komoriya@paken.org>
#
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
BACKUPDIRS="/home/ubuntu /etc /boot"
BACKUPTO="/home/backup"
EXPIREDAYS=60
EXECDIR=${0%/*}
EXCLUDEFILE=$EXECDIR/backup_exclude 

# delete old backup directories
echo "  Deleting old backup directories..."
DATE=`date +%Y%m%d`

for DIR in `ls $BACKUPTO | grep "_backup_"`
do
    BDATE=`echo $DIR | sed "s/_backup_//"`
    EXPIREDATE=`date +%Y%m%d -d "$EXPIREDAYS days ago"`
    if [ $BDATE -le $EXPIREDATE ]
    then
        echo "   deleting $BACKUPTO/$DIR"
        rm -rf $BACKUPTO/$DIR
    fi
done

# rsync options
OPTS="--force --delete-excluded --delete --backup --backup-dir=$BACKUPTO/_backup_$DATE -av"
if [ -f $EXCLUDEFILE ]; then
    OPTS="$OPTS --exclude-from=$EXCLUDEFILE"
fi

# execute backup
echo "  Executing backup with rsync..."
for dir in $BACKUPDIRS
do
    echo "  rsync $OPTS $dir $BACKUPTO"
    rsync $OPTS $dir $BACKUPTO
done

