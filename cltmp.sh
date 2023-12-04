#!/bin/sh
#
########################################################################
# cltmp: Cleanup Temporary and Cached Files
#
#  Description:
#  This script is designed to clean up temporary and cached files on Unix-like systems.
#  It includes specific operations for both macOS (Darwin) and Linux platforms, such as
#  removing old files from designated directories and clearing system caches.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  20230827 - Latest update with specific cleanup operations for macOS and Linux.
#  [Further version history truncated for brevity]
#
# Usage:
#  Run the script without any arguments:
#      cltmp.sh
#
#  The script automatically detects the operating system and performs the appropriate
#  cleanup actions. Ensure to have the necessary permissions before running.
#
########################################################################

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
    trash -ev
    test -d $HOME/Pictures && find $HOME/Pictures -type f -mtime +7 -exec trash -v {} \;
    test -d $HOME/Downloads && find $HOME/Documents -type f -mtime +7 -exec trash -v {} \;
    test -d $HOME/Downloads && find $HOME/Downloads -type f -mtime +3 -exec trash -v {} \;
    test -d $HOME/Desktop && find $HOME/Desktop -type f -mtime +3 -exec trash -v {} \;
    echo "Show trash contents..."
    trash -lv
  else
    test -d $HOME/Pictures && find $HOME/Pictures -type f -mtime +30 -exec rm -vf {} \;
    test -d $HOME/Pictures && find $HOME/Documents -type f -mtime +30 -exec rm -vf {} \;
    test -d $HOME/Downloads && find $HOME/Downloads -type f -mtime +7 -exec rm -vf {} \;
    test -d $HOME/Desktop && find $HOME/Desktop -type f -mtime +7 -exec rm -vf {} \;
  fi
else
  test -d $HOME/Pictures && find $HOME/Pictures -type f -mtime +30 -exec rm -vf {} \;
  test -d $HOME/Documents && find $HOME/Documents -type f -mtime +30 -exec rm -vf {} \;
  test -d $HOME/Downloads && find $HOME/Downloads -type f -mtime +7 -exec rm -vf {} \;
  test -d $HOME/Desktop && find $HOME/Desktop -type f -mtime +7 -exec rm -vf {} \;
fi
echo "cltmp (20230827) done."

