#!/bin/sh

echo -n "Server Resource Report at `/bin/hostname` on "
date "+%Y/%m/%d %T"
echo

echo "[dmesg Linux version]"
dmesg | grep "Linux version"
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

echo "[ps -H auxwww]"
ps -H auxwww
echo

echo "[ps axl --sort -vsize | head -20]"
ps axl --sort -vsize | head -20
echo

echo "[grep SSH attack /var/log/messages]"
grep "SSH attack" /var/log/messages
echo

echo "[egrep '(Fail|refuse)' /var/log/auth.log]"
egrep '(Fail|refuse)' /var/log/auth.log
echo

echo -n "End of Report at `/bin/hostname` on "
date "+%Y/%m/%d %T"
echo

