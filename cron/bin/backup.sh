#!/bin/sh
#
########################################################################
# Daily Backup Script
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.3 7/4,2011
#       Add remote sync function.
#  v1.2 4/27,2011
#       Compress mysqldump.
#  v1.1 4/18,2011
#       SVN dump.
#  v1.0 3/8,2011
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
    EXPIREDAYS=3
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
    while [ $# -gt 0 ]
    do
        BDATE=`echo $1 | sed "s/_backup_//"`
        EXPIREDATE=`date +%Y%m%d -d "$EXPIREDAYS days ago"`
        if [ $BDATE -le $EXPIREDATE ]
        then
            echo "deleting $BACKUPTO/$1"
            rm -rf $BACKUPTO/$1
        fi
        shift
    done
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
        rsync $OPTS $1 $BACKUPTO
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
    echo "mysqldump $1"
    mysqldump --add-drop-table --add-locks --password=$3 -u $2 \
        $1 > $BACKUPTO/mysqldump/$1.sql \
        && zip $BACKUPTO/mysqldump/$1.zip $BACKUPTO/mysqldump/$1.sql \
        && rm $BACKUPTO/mysqldump/$1.sql
    echo "Return code is $?"
}

dump_mysql() {
    echo -n "* Executing mysqldump on "
    date "+%Y/%m/%d %T"
    #get_mysqldump MYSQL_TABLE USERNAME PASSWORD
}

get_svndump() {
    echo "svndump $1"
    svnadmin dump $2 > $BACKUPTO/svndump/$1.dump \
        && zip $BACKUPTO/svndump/$1.zip $BACKUPTO/svndump/$1.dump \
        && rm $BACKUPTO/svndump/$1.dump
    echo "Return code is $?"
}

dump_svn() {
    echo -n "* Executing svndump on "
    date "+%Y/%m/%d %T"
    #get_svndump repo_id repo_path
}

mirror_to_remote() {
    if [ -d $BACKUPTO ]; then
        echo "rsync -avz --delete -e ssh $BACKUPTO root@$1:$2"
        ping -c 1 -i 3 $1 > /dev/null 2>&1 && rsync -avz --delete -e ssh $BACKUPTO root@$1:$2
        echo "Return code is $?"
    fi
}

remote_to_remote() {
    echo "rsync -avz /root/mnt/$1/home/backup /root/mnt/$2/home/remote/$1/"
    test -d /root/mnt/$1 || mkdir -p /root/mnt/$1
    test -d /root/mnt/$2 || mkdir -p /root/mnt/$2
    sshfs root@$1:/ /root/mnt/$1
    sshfs root@$2:/ /root/mnt/$2
    rsync -avz /root/mnt/$1/home/backup /root/mnt/$2/home/remote/$1/
    echo "Return code is $?"
    umount /root/mnt/$1
    umount /root/mnt/$2
}

remote_sync() {
    echo -n "* Executing remote sync on "
    date "+%Y/%m/%d %T"
    ping -c 1 -i 3 $1 > /dev/null 2>&1 && \
        ping -c 1 -i 3 $2 > /dev/null 2>&1 && \
            remote_to_remote $1 $2
}

backup_to_remote() {
    echo -n "* Executing backup to remote on "
    date "+%Y/%m/%d %T"
    #mirror_to_remote REMOTE_HOST REMOTE_TO
    #remote_sync SOURCE_HOST TARGET_HOST
}

main() {
    setup_environment
    get_resources
    purge_expires
    run_rsync
    #dump_mysql
    #dump_svn
    #backup_to_remote
}

main
