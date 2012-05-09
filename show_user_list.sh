#!/bin/sh

while read LINE
do
  if [ `echo $LINE | cut -f3 -d:` -ge 500 ]
  then
    echo $LINE | cut -f1 -d:
  fi
done < /etc/passwd
