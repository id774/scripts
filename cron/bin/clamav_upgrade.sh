#!/bin/sh
#
########################################################################
# ClamAV Auto Upgrade Script
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 11/10,2007
#       Add clamscan. Not delete source and object code.
#  v0.1 10/16,2007
#       New.
########################################################################
# Install:
# 1. sudo aptitude install clamav
# 2. install_clamav.sh
# 3. Install this script to cron.
########################################################################
TARGETDIRS="/"
LOGFILE=/var/log/clamscan.log
EXECDIR=${0%/*}
EXCLUDEFILE=$EXECDIR/clamscan_exclude 
test -d /usr/local/src/security || mkdir -p /usr/local/src/security
#test -d /usr/local/src/security/clamav && rm -rf /usr/local/src/security/clamav
cd /usr/local/src/security
svn co http://svn.clamav.net/svn/clamav-devel/trunk/ clamav
cd clamav
make clean
./configure
make
make install
test -f /usr/local/etc/freshclam.conf.base && cp /usr/local/etc/freshclam.conf.base /usr/local/etc/freshclam.conf
test -f /usr/local/etc/clamd.conf.base && cp /usr/local/etc/clamd.conf.base /usr/local/etc/clamd.conf
chmod 700 /usr/local/etc/freshclam.conf.base
chmod 700 /usr/local/etc/freshclam.conf
freshclam
if [ -s $EXCLUDEFILE ]; then
    for i in `cat $EXCLUDEFILE`
    do
        if [ $(echo "$i"|grep \/$) ]; then
            i=`echo $i|sed -e 's/^\([^ ]*\)\/$/\1/p' -e d`
            OPTS="${OPTS} --exclude-dir=$i"
        else
            OPTS="${OPTS} --exclude=$i"
        fi
    done
fi
for dir in $TARGETDIRS
do
    echo "clamscan ${dir} ${OPTS} -r -i -l ${LOGFILE}"
    clamscan ${dir} ${OPTS} -r -i -l ${LOGFILE}
done

