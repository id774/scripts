#!/bin/zsh

test -n "$1" || exit 1

sudo veracrypt -tc -t -k "" --protect-hidden no --fs-options utf8 /dev/$1 ~/mnt/$1
