#!/bin/sh

test -d $HOME/local || mkdir $HOME/local
test -d $HOME/local/github/scripts && cd $HOME/local/github/scripts && git pull && cd -
test -f $HOME/local/github/github.tar.gz && rm -f $HOME/local/github/github.tar.gz
test -d $HOME/local/github && tar czvf $HOME/local/github.tar.gz $HOME/local/github/
