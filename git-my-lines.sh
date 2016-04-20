#!/bin/sh

# http://qiita.com/kumagi/items/23db1b7b564db27b53e6
git ls-files | xargs -n1 git --no-pager blame -f -w | grep id774 | wc -l
