#!/bin/sh

THRESHOLD=0
test -f /etc/debian_version && THRESHOLD=1000
test -f /etc/redhat-release && THRESHOLD=500

show_userlist() {
  while read LINE
  do
    if [ `echo $LINE | cut -f3 -d:` -ge $THRESHOLD ]
    then
      echo $LINE | cut -f1 -d:
    fi
  done < /etc/passwd
}

test $THRESHOLD = 0 || show_userlist
