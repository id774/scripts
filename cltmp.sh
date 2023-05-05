#!/bin/sh

os=$(uname)

if [ "$os" = "Darwin" ]; then
  test -L ~/Desktop/場所が変更された項目 && rm -f ~/Desktop/場所が変更された項目
  touch ~/Documents/.localized
  touch ~/Downloads/.localized
  touch ~/Desktop/.localized
elif [ "$os" = "Linux" ]; then
  test -d /root/.cache && rm -vrf /root/.cache
  rm -vf ~/hardcopy.*
fi

rm -vf ~/wget-log*
rm -vf ~/.emacs.d/%backup%\~
rm -vf ~/%backup%\~
test -d ~/.gem && rm -vrf ~/.gem
test -d ~/.pip && rm -vrf ~/.pip
test -d ~/.npm && rm -vrf ~/.npm
test -d ~/tmp && rm -vrf ~/tmp/*
test -d ~/.tmp && find ~/.tmp -type f -mtime +7 -exec rm -vf {} \;
test -d ~/.emacs.d/tmp && find ~/.emacs.d/tmp -type f -mtime +30 -exec rm -vf {} \;
test -d ~/.emacs.d/backups && find ~/.emacs.d/backups -type f -mtime +30 -exec rm -vf {} \;
test -d ~/.emacs.d/auto-save-list && find ~/.emacs.d/auto-save-list -type f -mtime +30 -exec rm -vf {} \;
test -d ~/.emacs.d/tramp-auto-save && find ~/.emacs.d/tramp-auto-save -type f -mtime +30 -exec rm -vf {} \;
test -d ~/Pictures && find ~/Pictures -type f -mtime +30 -exec rm -vf {} \;
test -d ~/Downloads && find ~/Downloads -type f -mtime +30 -exec rm -vf {} \;
test -d ~/Desktop && find ~/Desktop -type f -mtime +30 -exec rm -vf {} \;
test -d ~/twitter_viewer/log && find ~/twitter_viewer/log -type f -mtime +7 -exec rm -vf {} \;
test -d ~/fastladder/log && find ~/fastladder/log -type f -mtime +7 -exec rm -vf {} \;
echo "cltmp (20230505) done."

