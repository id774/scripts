#!/bin/sh

test -d $HOME/.vmware || exit 1

cp -v $SCRIPTS/etc/vmware-config $HOME/.vmware/config

