#!/bin/sh
#
########################################################################
# ClamAV AutoScan Script
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.5 6/15,2011
#       Forked from clamav_upgrade.sh.
#  v0.4 10/22,2009
#       Upgrade repository svn to git.
#  v0.3 6/16,2009
#       Refactoring.
#  v0.2 11/10,2007
#       Add clamscan. Not delete source and object code.
#  v0.1 10/16,2007
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
