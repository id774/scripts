#!/bin/zsh

cd $HOME/local/github
sed -i -e 's/git:\/\//https:\/\//g' **/.git/config

