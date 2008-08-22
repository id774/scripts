#!/bin/sh

echo "TrueCrypt version=$1"
echo "mount-all=$2"
echo "fs-options=$3"

case "$1" in
  4)
    dmesg | grep sdb
    test -b /dev/sdb1 && sudo truecrypt -M $3 -u /dev/sdb1 ~/mnt/sdb
    test -b /dev/sdb1 || ( test -b /dev/sdb && sudo truecrypt -M $3 -u /dev/sdb ~/mnt/sdb )
    case "$2" in
      true)
        dmesg | grep sdc
        test -b /dev/sdc1 && sudo truecrypt -M $3 -u /dev/sdc1 ~/mnt/sdc
        test -b /dev/sdc1 || ( test -b /dev/sdc && sudo truecrypt -M $3 -u /dev/sdc ~/mnt/sdc )
        dmesg | grep sdd
        test -b /dev/sdd1 && sudo truecrypt -M $3 -u /dev/sdd1 ~/mnt/sdd
        test -b /dev/sdd1 || ( test -b /dev/sdd && sudo truecrypt -M $3 -u /dev/sdd ~/mnt/sdd )
        dmesg | grep sde
        test -b /dev/sde1 && sudo truecrypt -M $3 -u /dev/sde1 ~/mnt/sde
        test -b /dev/sde1 || ( test -b /dev/sde && sudo truecrypt -M $3 -u /dev/sde ~/mnt/sde )
        dmesg | grep sdf
        test -b /dev/sdf1 && sudo truecrypt -M $3 -u /dev/sdf1 ~/mnt/sdf
        test -b /dev/sdf1 || ( test -b /dev/sdf && sudo truecrypt -M $3 -u /dev/sdf ~/mnt/sdf )
        ;;
    esac
    ;;
  5)
    dmesg | grep sdb
    test -b /dev/sdb1 && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=$3 /dev/sdb1 ~/mnt/sdb
    test -b /dev/sdb1 || ( test -b /dev/sdb && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=$3 /dev/sdb ~/mnt/sdb )
    case "$2" in
      true)
        dmesg | grep sdc
        test -b /dev/sdc1 && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=$3 /dev/sdc1 ~/mnt/sdc
        test -b /dev/sdc1 || ( test -b /dev/sdc && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=$3 /dev/sdc ~/mnt/sdc )
        dmesg | grep sdd
        test -b /dev/sdd1 && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=$3 /dev/sdd1 ~/mnt/sdd
        test -b /dev/sdd1 || ( test -b /dev/sdd && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=$3 /dev/sdd ~/mnt/sdd )
        dmesg | grep sde
        test -b /dev/sde1 && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=$3 /dev/sde1 ~/mnt/sde
        test -b /dev/sde1 || ( test -b /dev/sde && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=$3 /dev/sde ~/mnt/sde )
        dmesg | grep sdf
        test -b /dev/sdf1 && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=$3 /dev/sdf1 ~/mnt/sdf
        test -b /dev/sdf1 || ( test -b /dev/sdf && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=$3 /dev/sdf ~/mnt/sdf )
        ;;
    esac
    ;;
esac
