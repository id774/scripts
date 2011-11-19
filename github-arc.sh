#!/bin/sh

test -d $HOME/local/github/scripts && cd $HOME/local/github/scripts && git pull && cd -
test -d $HOME/local/github/dot_zsh && cd $HOME/local/github/dot_zsh && git pull && cd -
test -d $HOME/local/github/dot_emacs && cd $HOME/local/github/dot_emacs && git pull && cd -
test -d $HOME/local/github/termtter-plugins && cd $HOME/local/github/termtter-plugins && git pull && cd -
test -d $HOME/local/github/intraweb-template && cd $HOME/local/github/intraweb-template && git pull && cd -
test -d $HOME/local/github/deferred-sync && cd $HOME/local/github/deferred-sync && git pull && cd -
test -d $HOME/local/github/instant-deployer && cd $HOME/local/github/instant-deployer && git pull && cd -
test -f $HOME/local/github/github.tar.gz && rm -f $HOME/local/github/github.tar.gz
test -d $HOME/local/github && tar czvf $HOME/local/github.tar.gz $HOME/local/github/
