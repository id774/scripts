#!/bin/bash

test -n "$1" && LOG_PATH=$1
test -n "$1" || LOG_PATH=/var/log/apache2
test -n "$2" && LOG_FILENAME=$2
test -n "$2" || LOG_FILENAME=ssl_access.log

echo "[Access Count]"
zgrep https $LOG_PATH/$LOG_FILENAME* | awk -F '"' '{print $2}' | awk '{print $2}' | sort | uniq -c | sort -n -r | head -n 100

echo "[Referer]"
zgrep https $LOG_PATH/$LOG_FILENAME* | cut -d " " -f11 | sort | uniq -c | sort -r | head -n 100

echo "[User Agent]"
zgrep https $LOG_PATH/$LOG_FILENAME* | awk -F '"' '{print $6}' | sort | uniq -c | sort -n -r | head -n 50

echo "[Browser]"
for UA in MSIE Firefox Chrome Safari; do COUNT=`zgrep 'https' $LOG_PATH/$LOG_FILENAME* | grep "$UA" | wc -l`; echo "$UA: $COUNT"; done

echo "[Daily Access]"
zgrep https $LOG_PATH/$LOG_FILENAME* | awk '{print $4}' | cut -b 2-12 | sort | uniq -c

echo "[Access By Time]"
grep https $LOG_PATH/$LOG_FILENAME* | awk '{print $4}' | cut -b 2-15 | sort | uniq -c

echo "[Recent Accesses]"
grep https $LOG_PATH/$LOG_FILENAME* | awk -F '"' '{print $2}' | awk '{print $2}' | sort | uniq -c | sort -n -r | head -n 100

echo "[Recent Referer]"
grep https $LOG_PATH/$LOG_FILENAME* | cut -d " " -f11 | sort | uniq -c | sort -r | head -n 100

