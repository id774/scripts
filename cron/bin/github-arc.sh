#!/bin/bash

HOME=/root
USER_HOME=/home/admin

du -h --max-depth=1 $USER_HOME/local/github
du -h --max-depth=1 $USER_HOME/local/git
test -f $HOME/local/github.tar.gz && rm -f $HOME/local/github.tar.gz
test -d $HOME/local/github && test -d $USER_HOME/local/github && tar czvf $HOME/local/github.tar.gz $USER_HOME/local/github
