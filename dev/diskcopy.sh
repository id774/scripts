#!/bin/sh

JOBLOG=~/diskcopy.log
test -n "$1" && SOURCE_DRV=$1
test -n "$1" || SOURCE_DRV=~/mnt/sdc
test -n "$2" && TARGET_DRV=$2
test -n "$2" || TARGET_DRV=~/mnt/sdb
test -n "$3" && ADMIN_MAIL_ADDRESS=$3
test -n "$3" || ADMIN_MAIL_ADDRESS=xxxxxx@gmail.com

echo "diskcopy start.">$JOBLOG

echo -n "Diskcopy job started at `/bin/hostname` on ">>$JOBLOG
date "+%Y/%m/%d %T">>$JOBLOG
case "$ADMIN_MAIL_ADDRESS" in
  *@*)
    cat $JOBLOG | mail -s "[cron-log][`/bin/hostname`] Diskcopy Log" $ADMIN_MAIL_ADDRESS
    ;;
esac

cp -av $SOURCE_DRV/user1 $TARGET_DRV
echo "Return code is $?">>$JOBLOG
echo -n "Diskcopy user1 completed at `/bin/hostname` on ">>$JOBLOG
date "+%Y/%m/%d %T">>$JOBLOG
case "$ADMIN_MAIL_ADDRESS" in
  *@*)
    cat $JOBLOG | mail -s "[cron-log][`/bin/hostname`] Diskcopy Log" $ADMIN_MAIL_ADDRESS
    ;;
esac

cp -av $SOURCE_DRV/user2 $TARGET_DRV
echo "Return code is $?">>$JOBLOG
echo -n "Diskcopy user2 completed at `/bin/hostname` on ">>$JOBLOG
date "+%Y/%m/%d %T">>$JOBLOG
case "$ADMIN_MAIL_ADDRESS" in
  *@*)
    cat $JOBLOG | mail -s "[cron-log][`/bin/hostname`] Diskcopy Log" $ADMIN_MAIL_ADDRESS
    ;;
esac

cp -av $SOURCE_DRV/user3 $TARGET_DRV
echo "Return code is $?">>$JOBLOG
echo -n "Diskcopy user3 completed at `/bin/hostname` on ">>$JOBLOG
date "+%Y/%m/%d %T">>$JOBLOG
case "$ADMIN_MAIL_ADDRESS" in
  *@*)
    cat $JOBLOG | mail -s "[cron-log][`/bin/hostname`] Diskcopy Log" $ADMIN_MAIL_ADDRESS
    ;;
esac

cp -av $SOURCE_DRV/largefiles $TARGET_DRV
echo "Return code is $?">>$JOBLOG
echo -n "Diskcopy largefiles completed at `/bin/hostname` on ">>$JOBLOG
date "+%Y/%m/%d %T">>$JOBLOG
df -T>>$JOBLOG
case "$ADMIN_MAIL_ADDRESS" in
  *@*)
    cat $JOBLOG | mail -s "[cron-log][`/bin/hostname`] Diskcopy Log" $ADMIN_MAIL_ADDRESS
    ;;
esac

sudo test -b /dev/sdb && smartctl -a /dev/sdb
sudo test -b /dev/sdc && smartctl -a /dev/sdc
