#!/bin/sh

JOBLOG=/var/log/plagger2ch2twitter.log

echo -n "*** $0: Job start at `/bin/hostname` on ">>$JOBLOG 2>&1
date "+%Y/%m/%d %T">>$JOBLOG 2>&1

/usr/local/bin/plagger -c $HOME/yaml/2ch_hatenadiary.yaml>>$JOBLOG 2>&1
/usr/local/bin/plagger -c $HOME/yaml/2ch_tw.yaml>>$JOBLOG 2>&1
/usr/local/bin/plagger -c $HOME/yaml/2ch_tw2.yaml>>$JOBLOG 2>&1
/usr/local/bin/plagger -c $HOME/yaml/linuxcafe2twitter.yaml>>$JOBLOG 2>&1
/usr/local/bin/plagger -c $HOME/yaml/hackerscafeblog2twitter.yaml>>$JOBLOG 2>&1
/usr/local/bin/plagger -c $HOME/yaml/commitlog2twitter.yaml>>$JOBLOG 2>&1
/usr/local/bin/plagger -c $HOME/yaml/2ch_hashigotan.yaml>>$JOBLOG 2>&1
/usr/local/bin/plagger -c $HOME/yaml/coderepos2twitter.yaml>>$JOBLOG 2>&1
/usr/local/bin/plagger -c $HOME/yaml/geekhouse2twitter.yaml>>$JOBLOG 2>&1
/usr/local/bin/plagger -c $HOME/yaml/hatenabookmark2geekhouse.yaml>>$JOBLOG 2>&1

echo -n "*** $0: End of Job at `/bin/hostname` on ">>$JOBLOG 2>&1
date "+%Y/%m/%d %T">>$JOBLOG 2>&1
echo>>$JOBLOG 2>&1

