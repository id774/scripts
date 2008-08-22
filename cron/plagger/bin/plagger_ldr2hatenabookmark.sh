#!/bin/sh

JOBLOG=/var/log/plaggersbm.log

echo -n "*** $0: Job start at `/bin/hostname` on ">>$JOBLOG 2>&1
date "+%Y/%m/%d %T">>$JOBLOG 2>&1

/usr/local/bin/plagger -c $HOME/yaml/ldr2hatenabookmark_25_netwatch.yaml>>$JOBLOG 2>&1
/usr/local/bin/plagger -c $HOME/yaml/ldr2hatenabookmark.yaml>>$JOBLOG 2>&1

echo -n "*** $0: End of Job at `/bin/hostname` on ">>$JOBLOG 2>&1
date "+%Y/%m/%d %T">>$JOBLOG 2>&1
echo>>$JOBLOG 2>&1

