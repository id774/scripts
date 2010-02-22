#!/bin/sh

test -d $HOME/.ssh || mkdir $HOME/.ssh
cp $SCRIPTS/etc/dot-ssh-config $HOME/.ssh/config

