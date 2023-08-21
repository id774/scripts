#!/bin/sh

os=$(uname)

if [ "$os" = "Darwin" ]; then
  test -L "$HOME/Desktop/場所が変更された項目" && rm -f "$HOME/Desktop/場所が変更された項目"
  test -d $HOME/Pictures && touch $HOME/Pictures/.localized
  test -d $HOME/Documents && touch $HOME/Documents/.localized
  test -d $HOME/Downloads && touch $HOME/Downloads/.localized
  test -d $HOME/Desktop && touch $HOME/Desktop/.localized
  test -d $HOME/tmp && find $HOME/tmp -type f -mtime +3 -exec rm -vf {} \;
elif [ "$os" = "Linux" ]; then
  test -d /root/.cache && rm -vrf /root/.cache
  rm -vf $HOME/hardcopy.*
  test -d $HOME/tmp && find $HOME/tmp -type f -mtime +1 -exec rm -vf {} \;
fi

rm -vf "$HOME/wget-log*"
rm -vf "$HOME/.emacs.d/%backup%$HOME"
rm -vf "$HOME/%backup%$HOME"
test -d $HOME/.gem && rm -vrf $HOME/.gem
test -d $HOME/.pip && rm -vrf $HOME/.pip
test -d $HOME/.npm && rm -vrf $HOME/.npm
test -d $HOME/.tmp && find $HOME/.tmp -type f -mtime +7 -exec rm -vf {} \;
test -d $HOME/.emacs.d/tmp && find $HOME/.emacs.d/tmp -type f -mtime +30 -exec rm -vf {} \;
test -d $HOME/.emacs.d/backups && find $HOME/.emacs.d/backups -type f -mtime +30 -exec rm -vf {} \;
test -d $HOME/.emacs.d/auto-save-list && find $HOME/.emacs.d/auto-save-list -type f -mtime +30 -exec rm -vf {} \;
test -d $HOME/.emacs.d/tramp-auto-save && find $HOME/.emacs.d/tramp-auto-save -type f -mtime +30 -exec rm -vf {} \;
test -d $HOME/twitter_viewer/log && find $HOME/twitter_viewer/log -type f -mtime +7 -exec rm -vf {} \;
test -d $HOME/fastladder/log && find $HOME/fastladder/log -type f -mtime +7 -exec rm -vf {} \;

if [ "$os" = "Darwin" ]; then
  if type trash &> /dev/null
  then
    test -d $HOME/Pictures && find $HOME/Pictures -type f -mtime +30 -exec trash -v {} \;
    test -d $HOME/Downloads && find $HOME/Downloads -type f -mtime +7 -exec trash -v {} \;
    test -d $HOME/Desktop && find $HOME/Desktop -type f -mtime +7 -exec trash -v {} \;
  else
    test -d $HOME/Pictures && find $HOME/Pictures -type f -mtime +30 -exec rm -vf {} \;
    test -d $HOME/Downloads && find $HOME/Downloads -type f -mtime +7 -exec rm -vf {} \;
    test -d $HOME/Desktop && find $HOME/Desktop -type f -mtime +7 -exec rm -vf {} \;
  fi
else
  test -d $HOME/Pictures && find $HOME/Pictures -type f -mtime +30 -exec rm -vf {} \;
  test -d $HOME/Downloads && find $HOME/Downloads -type f -mtime +7 -exec rm -vf {} \;
  test -d $HOME/Desktop && find $HOME/Desktop -type f -mtime +7 -exec rm -vf {} \;
fi
echo "cltmp (20230822) done."

