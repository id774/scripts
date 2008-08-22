#!/bin/sh

test -n "$1" && SOURCE_DRV=$1
test -n "$1" || SOURCE_DRV=~/mnt/sdc
test -n "$2" && TARGET_DRV=$2
test -n "$2" || TARGET_DRV=~/mnt/sdb
test -n "$3" && ADMIN_MAIL_ADDRESS=$3
test -n "$3" || ADMIN_MAIL_ADDRESS=xxxxxx@gmail.com

echo "diskcopy start.">~/diskcopy.log

cp -av $SOURCE_DRV/user1 $TARGET_DRV
echo "user1 completed.">>~/diskcopy.log
case "$ADMIN_MAIL_ADDRESS" in
  *@*)
    cat ~/diskcopy.log | mail -s "[cron-log][`/bin/hostname`] Diskcopy Log" $ADMIN_MAIL_ADDRESS
    ;;
esac

cp -av $SOURCE_DRV/user2 $TARGET_DRV
echo "user2 completed.">>~/diskcopy.log
case "$ADMIN_MAIL_ADDRESS" in
  *@*)
    cat ~/diskcopy.log | mail -s "[cron-log][`/bin/hostname`] Diskcopy Log" $ADMIN_MAIL_ADDRESS
    ;;
esac

cp -av $SOURCE_DRV/user3 $TARGET_DRV
echo "user3 completed.">>~/diskcopy.log
case "$ADMIN_MAIL_ADDRESS" in
  *@*)
    cat ~/diskcopy.log | mail -s "[cron-log][`/bin/hostname`] Diskcopy Log" $ADMIN_MAIL_ADDRESS
    ;;
esac

cp -av $SOURCE_DRV/largefiles $TARGET_DRV
echo "largefiles completed.">>~/diskcopy.log
case "$ADMIN_MAIL_ADDRESS" in
  *@*)
    cat ~/diskcopy.log | mail -s "[cron-log][`/bin/hostname`] Diskcopy Log" $ADMIN_MAIL_ADDRESS
    ;;
esac

