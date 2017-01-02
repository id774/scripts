#!/bin/sh

test -d $HOME/.karabiner.d/configuration || mkdir -p $HOME/.karabiner.d/configuration
cp $SCRIPTS/dot_files/dot_karabiner.d/configuration/karabiner.json $HOME/.karabiner.d/configuration/karabiner.json
