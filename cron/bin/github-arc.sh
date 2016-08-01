#!/bin/bash

HOME=/root
USER_HOME=/home/admin

test -f $HOME/local/github.tar.gz && rm -f $HOME/local/github.tar.gz
test -d $USER_HOME/local/github && tar czvf $HOME/local/github.tar.gz $USER_HOME/local/github > /dev/null
du -h --max-depth=1 $USER_HOME/local/github
du -h --max-depth=1 $USER_HOME/local/git
