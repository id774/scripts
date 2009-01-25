#!/bin/sh

test -f /home/git/coderepos/coderepos.tar.gz && rm /home/git/coderepos/coderepos.tar.gz
cd /home/git/coderepos/share
git svn rebase
cd ..
tar czvf /home/git/coderepos/coderepos.tar.gz share/ >/dev/null
chmod 640 /home/git/coderepos/coderepos.tar.gz
chown root:admin /home/git/coderepos/coderepos.tar.gz

