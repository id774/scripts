#!/bin/sh

# http://qiita.com/Night___/items/359ff81f358968567a45
git log --numstat --pretty="%H" $* --no-merges | awk 'NF==3 {plus+=$1; minus+=$2} END {printf("%d (+%d, -%d)\n", plus+minus, plus, minus)}'
