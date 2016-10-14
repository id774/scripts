#!/bin/sh

rm -vrf ~/tmp/*
rm -vrf ~/.tmp/*
rm -vf ~/hardcopy.*
rm -vf ~/wget-log*
rm -vf ~/.emacs.d/%backup%\~
rm -vf ~/%backup%\~
rm -vrf ~/.emacs.d/tmp/*
rm -vrf ~/.emacs.d/backups/*
rm -vrf ~/.emacs.d/auto-save-list/.saves*
rm -vrf ~/.emacs.d/tramp-auto-save/*
rm -vf ~/Pictures/*.*g
rm -vf ~/Pictures/*.*G
rm -vf ~/Downloads/*
rm -vf ~/Desktop/*
test -d ~/.gem && rm -vrf ~/.gem
test -d ~/.pip && rm -vrf ~/.pip
test -d ~/.npm && rm -vrf ~/.npm
test -d /root/.cache && rm -vrf /root/.cache
test -f ~/twitter_viewer/log/twitter1.log && rm -vf ~/twitter_viewer/log/*.log
test -f ~/fastladder/log/development.log && rm -vf ~/fastladder/log/development.log
echo "cltmp (20161014) done."

