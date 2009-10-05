#!/bin/sh

FIND=/usr/bin/find
EXIFTOOL=/usr/bin/exiftool
JHEAD=/usr/bin/jhead
PATH=$1

$FIND $PATH -type f | while read FILE
do
  RESULT=`$EXIFTOOL -gps:GPSLatitude $FILE`
  if test -n "$RESULT"
  then
    $JHEAD -purejpg $FILE
    echo $FILE
  fi
done
