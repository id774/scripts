#!/bin/bash

echo "[Access Count]"
zgrep https /var/log/apache2/ssl_access.log* | awk -F '"' '{print $2}' | awk '{print $2}' | sort | uniq -c | sort -n -r | head -n 100

echo "[Referer]"
zgrep https /var/log/apache2/ssl_access.log* | cut -d " " -f11 | sort | uniq -c | sort -r | head -n 100

echo "[User Agent]"
zgrep https /var/log/apache2/ssl_access.log* | awk -F '"' '{print $6}' | sort | uniq -c | sort -n -r | head -n 50

echo "[Browser]"
for UA in MSIE Firefox Chrome Safari; do COUNT=`zgrep 'https' /var/log/apache2/ssl_access.log* | grep "$UA" | wc -l`; echo "$UA: $COUNT"; done

echo "[Daily Access]"
zgrep https /var/log/apache2/ssl_access.log* | awk '{print $4}' | cut -b 2-12 | sort | uniq -c

echo "[Access By Time]"
grep https /var/log/apache2/ssl_access.log* | awk '{print $4}' | cut -b 2-15 | sort | uniq -c

