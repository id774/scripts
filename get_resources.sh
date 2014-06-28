#!/bin/sh

echo -n "Server Resource Report on `/bin/hostname` at "
date "+%Y/%m/%d %T"
echo

echo "[GNU/Linux version]"
dmesg | grep "Linux version"
which lsb_release > /dev/null && lsb_release -a
echo

echo "[uname -a]"
uname -a
echo

echo "[uptime]"
uptime
echo

echo "[cpuinfo]"
cat /proc/cpuinfo
echo

echo "[meminfo]"
cat /proc/meminfo
echo

echo "[free -t]"
free -t
echo

echo "[vmstat -n]"
vmstat -n
echo

echo "[df -P -T]"
df -P -T
echo

echo "[ifconfig]"
ifconfig
echo

echo "[netstat -s]"
netstat -s
echo

echo "[netstat -an]"
netstat -an
echo

echo "[w]"
w
echo

echo "[top -b -n 1]"
top -b -n 1
echo

echo "[lsmod]"
lsmod
echo

echo "[ps -H auxZwww]"
ps -H auxZwww
echo

echo "[ps axl --sort -vsize | head -20]"
ps axl --sort -vsize | head -20
echo

echo "[netstat -tan | grep ':80 ' | awk '{print $6}' | sort | uniq -c]"
netstat -tan | grep ':80 ' | awk '{print $6}' | sort | uniq -c
echo

echo "[ntpq -pn]"
ntpq -pn
echo

echo "[grep Accepted /var/log/auth.log]"
grep "Accepted" /var/log/auth.log
echo

echo "[grep attack /var/log/messages]"
grep " attack" /var/log/messages
echo

echo "[egrep '(Fail|refuse)' /var/log/auth.log]"
egrep '(Fail|refuse)' /var/log/auth.log
echo

echo "[grep WARNING /var/log/fail2ban.log]"
grep "WARNING" /var/log/fail2ban.log
echo

echo -n "End of Report at `/bin/hostname` on "
date "+%Y/%m/%d %T"
echo

