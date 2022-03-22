#!/bin/zsh

test -n "$1" && SOURCE_DRV=$1
test -n "$1" || SOURCE_DRV=$HOME/mnt/sdf
test -n "$2" && TARGET_DRV=$2
test -n "$2" || TARGET_DRV=$HOME/mnt/sdg
test -n "$3" && ADMIN_MAIL_ADDRESS=$3
test -n "$3" || ADMIN_MAIL_ADDRESS=sysad.idnanashi@gmail.com

test -d $SOURCE_DRV || exit 1
test -d $TARGET_DRV || exit 1

JOBLOG=$TMP/diskcheck.log

echo "diskcheck start.">$JOBLOG

echo -n "Diskcheck job started at `/bin/hostname` on ">>$JOBLOG
date "+%Y/%m/%d %T">>$JOBLOG

echo "Checking copy (DRY-RUN) from $SOURCE_DRV/user1 to $TARGET_DRV">>$JOBLOG
rsync --dry-run -avz --no-o --no-g --delete $SOURCE_DRV/user1 $TARGET_DRV/>>$JOBLOG
echo "Return code is $?">>$JOBLOG
echo -n "Diskcheck user1 completed at `/bin/hostname` on ">>$JOBLOG
date "+%Y/%m/%d %T">>$JOBLOG
echo "Checking copy (DRY-RUN) from $SOURCE_DRV/user2 to $TARGET_DRV">>$JOBLOG
rsync --dry-run -avz --no-o --no-g --delete $SOURCE_DRV/user2 $TARGET_DRV/>>$JOBLOG
echo "Return code is $?">>$JOBLOG
echo -n "Diskcheck user2 completed at `/bin/hostname` on ">>$JOBLOG
date "+%Y/%m/%d %T">>$JOBLOG
echo "Checking copy (DRY-RUN) from $SOURCE_DRV/user3 to $TARGET_DRV">>$JOBLOG
rsync --dry-run -avz --no-o --no-g --delete $SOURCE_DRV/user3 $TARGET_DRV/>>$JOBLOG
echo "Return code is $?">>$JOBLOG
echo -n "Diskcheck user3 completed at `/bin/hostname` on ">>$JOBLOG
date "+%Y/%m/%d %T">>$JOBLOG
echo "Checking copy (DRY-RUN) from $SOURCE_DRV/largefiles to $TARGET_DRV">>$JOBLOG
rsync --dry-run -avz --no-o --no-g --delete $SOURCE_DRV/largefiles $TARGET_DRV/>>$JOBLOG
echo "Return code is $?">>$JOBLOG
echo -n "Diskcheck largefiles completed at `/bin/hostname` on ">>$JOBLOG
date "+%Y/%m/%d %T">>$JOBLOG

case "$ADMIN_MAIL_ADDRESS" in
  *@*)
    cat -v $JOBLOG | nkf -w | mail -s "[admin][`/bin/hostname`] Diskcheck Log" $ADMIN_MAIL_ADDRESS
    ;;
esac
