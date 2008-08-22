#!/bin/zsh

chmod -R u+rw,g+r,g-w,o+r,o-w $SCRIPTS/*
chmod -R u+x,g+x,o+x $SCRIPTS/**/*.sh $SCRIPTS/**/*.py $SCRIPTS/**/*.rb

