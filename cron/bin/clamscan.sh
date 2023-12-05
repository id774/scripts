#!/bin/sh
#
########################################################################
# ClamAV AutoScan Script
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.5 2011-06-15
#       Forked from clamav_upgrade.sh.
#  v0.4 2009-10-22
#       Upgrade repository svn to git.
#  v0.3 2009-06-16
#       Refactoring.
#  v0.2 2007-11-10
#       Add clamscan. Not delete source and object code.
#  v0.1 2007-10-16
#       New.
########################################################################
TARGETDIRS="/"
LOGFILE=/var/log/clamav/clamav.log
EXECDIR=${0%/*}
EXCLUDEFILE=$EXECDIR/clamscan_exclude

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
