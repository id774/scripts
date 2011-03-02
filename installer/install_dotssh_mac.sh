#!/bin/sh

test -d $HOME/.ssh || mkdir $HOME/.ssh
cp $SCRIPTS/dot_files/dot_ssh_config $HOME/.ssh/config

