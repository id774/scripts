#!/bin/sh
#
########################################################################
# Daily Backup Script
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.8 3/8,2011
#       Refactoring.
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

setup_environment() {
    BACKUPDIRS="/home/debian /home/tiarra /home/plagger /var/lib/rails /var/www/html /root /etc /boot"
    BACKUPTO="/home/backup"
    EXPIREDAYS=10
    EXECDIR=${0%/*}
    EXCLUDEFILE=$EXECDIR/backup_exclude 
    DATE=`date +%Y%m%d`
}

get_resources() {
    uname -a
    uptime
    free -t
    df -P -T
    test -b /dev/sda && smartctl -a /dev/sda
    test -b /dev/hda && smartctl -a /dev/hda
}

purge_expire_dir() {
    BDATE=`echo $1 | sed "s/_backup_//"`
    EXPIREDATE=`date +%Y%m%d -d "$EXPIREDAYS days ago"`
    if [ $BDATE -le $EXPIREDATE ]
    then
        echo "deleting $BACKUPTO/$1"
        rm -rf $BACKUPTO/$DIR
    fi
}

purge_expires() {
    echo -n "* Deleting old backup directories on "
    date "+%Y/%m/%d %T"
    purge_expire_dir `ls $BACKUPTO | grep "_backup_"`
}

rsync_options() {
    OPTS="--force --delete-excluded --delete --backup --backup-dir=$BACKUPTO/_backup_$DATE -av"
    if [ -f $EXCLUDEFILE ]; then
        OPTS="$OPTS --exclude-from=$EXCLUDEFILE"
    fi
}

exec_rsync() {
    while [ $# -gt 0 ]
    do
        echo "rsync $OPTS $1 $BACKUPTO"
        #rsync $OPTS $1 $BACKUPTO
        echo "Return code is $?"
        shift
    done
}

run_rsync() {
    rsync_options
    echo -n "* Executing backup with rsync on "
    date "+%Y/%m/%d %T"
    exec_rsync $BACKUPDIRS
}

get_mysqldump() {
    echo -n "mysqldump $1"
    mysqldump --add-drop-table --add-locks --password=$2 -u $1 \
        $1 > $BACKUPTO/mysqldump/$1.sql
}

dump_mysql() {
    echo -n "* Executing mysqldump on "
    date "+%Y/%m/%d %T"
    #get_mysqldump MYSQL_TABLE PASSWORD
}

mirror_to_remote() {
    if [ -d $BACKUPTO ]; then
        echo -n "rsync -avz --delete -e ssh $BACKUPTO root@$1:$2"
        ping -c 1 -i 3 $1 > /dev/null 2>&1 && rsync -avz --delete -e ssh $BACKUPTO root@$1:$2
    fi
}

backup_to_remote() {
    echo -n "* Executing backup to remote on "
    date "+%Y/%m/%d %T"
    #mirror_to_remote REMOTE_HOST REMOTE_TO
}

main() {
    setup_environment
    get_resources
    purge_expires
    run_rsync
    #dump_mysql
    #backup_to_remote
}

main
