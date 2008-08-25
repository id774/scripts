#!/bin/sh

cd $HOME/local/github/scripts && git pull && cd -
rsync -av --delete $HOME/local/github/scripts $HOME/svnwork/id774
rm -rf $HOME/svnwork/id774/scripts/.git

cd $HOME/local/github/scratch && git pull && cd -
rsync -av --delete $HOME/local/github/scratch $HOME/svnwork/id774
rm -rf $HOME/svnwork/id774/scratch/.git

cd $HOME/svnwork/id774
svk add -v .
svk ci -m "clone from github"
svk sm -m "clone from github" //id774 //mirror/id774

