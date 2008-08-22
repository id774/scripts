#!/bin/sh

MOUNTALL=false
FSOPTIONS=utf8

case "$1" in
  *ar*|*ra*)
    MOUNTALL=true
    FSOPTIONS=ro,utf8
    ;;
  *a*)
    MOUNTALL=true
    ;;
  *r*)
    FSOPTIONS=ro,utf8
    ;;
esac

test -f ~/local/`/bin/hostname`.tc && test -d ~/mnt/tc && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=utf8 ~/local/`/bin/hostname`.tc ~/mnt/tc
test -x $SCRIPTS/tcmount_plus.sh && $SCRIPTS/tcmount_plus.sh 5 $MOUNTALL $FSOPTIONS
