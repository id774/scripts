#!/bin/sh

rm -vf ~/hardcopy.*
rm -vf ~/wget-log*
rm -vf ~/.emacs.d/%backup%\~
rm -vf ~/%backup%\~
test -d /root/.cache && rm -vrf /root/.cache
test -d ~/.gem && rm -vrf ~/.gem
test -d ~/.pip && rm -vrf ~/.pip
test -d ~/.npm && rm -vrf ~/.npm
test -d ~/tmp && rm -vrf ~/tmp/*
test -d ~/.tmp && find ~/.tmp -type f -mtime +3 -exec rm -vf {} \;
test -d ~/.emacs.d/tmp && find ~/.emacs.d/tmp -type f -mtime +7 -exec rm -vf {} \;
test -d ~/.emacs.d/backups && find ~/.emacs.d/backups -type f -mtime +7 -exec rm -vf {} \;
test -d ~/.emacs.d/auto-save-list && find ~/.emacs.d/auto-save-list -type f -mtime +7 -exec rm -vf {} \;
test -d ~/.emacs.d/tramp-auto-save && find ~/.emacs.d/tramp-auto-save -type f -mtime +7 -exec rm -vf {} \;
test -d ~/Pictures && find ~/Pictures -type f -mtime +7 -exec rm -vf {} \;
test -d ~/Downloads && find ~/Downloads -type f -mtime +7 -exec rm -vf {} \;
test -d ~/Desktop && find ~/Desktop -type f -mtime +7 -exec rm -vf {} \;
test -d ~/twitter_viewer/log && find ~/twitter_viewer/log -type f -mtime +3 -exec rm -vf {} \;
test -d ~/fastladder/log && find ~/fastladder/log -type f -mtime +3 -exec rm -vf {} \;
echo "cltmp (20200824) done."

