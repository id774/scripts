#!/bin/sh

test -f $HOME/local/git/coderepos.tar.gz && rm $HOME/local/git/coderepos.tar.gz
cd $HOME/local/git/share
git svn rebase
cd ..
tar czvf $HOME/local/git/coderepos.tar.gz share/

