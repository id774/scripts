#!/bin/sh

JOBLOG=$TMP/diskcopy.log
FILELIST=$TMP/filelist.log

test -n "$1" && SOURCE_DRV=$1
test -n "$1" || SOURCE_DRV=$HOME/mnt/sdc
test -n "$2" && TARGET_DRV=$2
test -n "$2" || TARGET_DRV=$HOME/mnt/sdb
test -n "$3" && ADMIN_MAIL_ADDRESS=$3
test -n "$3" || ADMIN_MAIL_ADDRESS=root

echo "diskcopy start.">$JOBLOG

echo -n "Diskcopy job started at `/bin/hostname` on ">>$JOBLOG
date "+%Y/%m/%d %T">>$JOBLOG
case "$ADMIN_MAIL_ADDRESS" in
  *@*)
    cat -v $JOBLOG | nkf -w | mail -s "[admin][`/bin/hostname`] Diskcopy Log" $ADMIN_MAIL_ADDRESS
    ;;
esac

echo "Copying from $SOURCE_DRV/user1 to $TARGET_DRV">>$JOBLOG
rsync -av $SOURCE_DRV/user1 $TARGET_DRV > $FILELIST 2>&1
echo "Return code is $?">>$JOBLOG
echo -n "Diskcopy user1 completed at `/bin/hostname` on ">>$JOBLOG
date "+%Y/%m/%d %T">>$JOBLOG
case "$ADMIN_MAIL_ADDRESS" in
  *@*)
    cat -v $JOBLOG | nkf -w | mail -s "[admin][`/bin/hostname`] Diskcopy Log" $ADMIN_MAIL_ADDRESS
    ;;
esac

echo "Copying from $SOURCE_DRV/user2 to $TARGET_DRV">>$JOBLOG
rsync -av $SOURCE_DRV/user2 $TARGET_DRV >> $FILELIST 2>&1
echo "Return code is $?">>$JOBLOG
echo -n "Diskcopy user2 completed at `/bin/hostname` on ">>$JOBLOG
date "+%Y/%m/%d %T">>$JOBLOG
case "$ADMIN_MAIL_ADDRESS" in
  *@*)
    cat -v $JOBLOG | nkf -w | mail -s "[admin][`/bin/hostname`] Diskcopy Log" $ADMIN_MAIL_ADDRESS
    ;;
esac

echo "Copying from $SOURCE_DRV/user3 to $TARGET_DRV">>$JOBLOG
rsync -av $SOURCE_DRV/user3 $TARGET_DRV >> $FILELIST 2>&1
echo "Return code is $?">>$JOBLOG
echo -n "Diskcopy user3 completed at `/bin/hostname` on ">>$JOBLOG
date "+%Y/%m/%d %T">>$JOBLOG
case "$ADMIN_MAIL_ADDRESS" in
  *@*)
    cat -v $JOBLOG | nkf -w | mail -s "[admin][`/bin/hostname`] Diskcopy Log" $ADMIN_MAIL_ADDRESS
    ;;
esac


echo "Copying from $SOURCE_DRV/largefiles to $TARGET_DRV">>$JOBLOG
rsync -av $SOURCE_DRV/largefiles $TARGET_DRV >> $FILELIST 2>&1
echo "Return code is $?">>$JOBLOG
echo -n "Diskcopy largefiles completed at `/bin/hostname` on ">>$JOBLOG
date "+%Y/%m/%d %T">>$JOBLOG
df -T>>$JOBLOG
case "$ADMIN_MAIL_ADDRESS" in
  *@*)
    cat -v $JOBLOG | nkf -w | mail -s "[admin][`/bin/hostname`] Diskcopy Log" $ADMIN_MAIL_ADDRESS
    ;;
esac

