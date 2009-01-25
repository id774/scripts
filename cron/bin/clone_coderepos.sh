#!/bin/sh

test -f $HOME/local/git/coderepos.tar.gz && rm $HOME/local/git/coderepos.tar.gz
cd $HOME/local/git/share
git svn rebase
cd ..
tar czvf $HOME/local/git/coderepos.tar.gz share/ >/dev/null

cp $HOME/local/git/coderepos.tar.gz /home/plagger/local/git/coderepos.tar.gz
chmod 640 /home/plagger/local/git/coderepos.tar.gz
chown plagger:plagger /home/plagger/local/git/coderepos.tar.gz

