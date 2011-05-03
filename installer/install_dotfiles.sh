#!/bin/sh
#
########################################################################
# Install dot_files
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 5/3,2011
#       Include .toprc.
#  v1.0 3/2,2010
#       Refactoring.
########################################################################

test -n "$1" && DEFAULT_KEYMAPFILE=$1
test -n "$1" || DEFAULT_KEYMAPFILE=dot_xmodmaprc_hhklite2

case $OSTYPE in
  *darwin*)
    OPTIONS=-v
    ;;
  *)
    OPTIONS=-vd
    ;;
esac

for DOT_FILES in zshrc screenrc vimrc gvimrc gitconfig gitignore toprc
do
  test -d /var/root            && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /var/root/.$DOT_FILES
  test -d /Users/mac           && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /Users/mac/.$DOT_FILES
  test -d /Users/apple         && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /Users/apple/.$DOT_FILES
  test -d /Users/demo          && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /Users/demo/.$DOT_FILES
  test -d /Users/work          && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /Users/work/.$DOT_FILES
  test -d /root                && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /root/.$DOT_FILES
  test -d /home/debian         && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /home/debian/.$DOT_FILES
  test -d /home/ubuntu         && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /home/ubuntu/.$DOT_FILES
  test -d /home/centos         && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /home/centos/.$DOT_FILES
  test -d /home/admin          && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /home/admin/.$DOT_FILES
  test -d /etc/skel            && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /etc/skel/.$DOT_FILES
  test -d /home/plagger        && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /home/plagger/.$DOT_FILES
  test -d /home/twitter        && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /home/twitter/.$DOT_FILES
  test -d /home/testuser       && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /home/testuser/.$DOT_FILES
  test -d /var/lib/postgresql  && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /var/lib/postgresql/.$DOT_FILES
  test -d /usr/lib/oracle/xe   && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /usr/lib/oracle/xe/.$DOT_FILES
  test -d /export/home/solaris && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /export/home/solaris/.$DOT_FILES
done

test -f ~/.zshrc.zwc && rm -f ~/.zshrc.zwc
cd
zsh -c 'zcompile ~/.zshrc'

if [ -d /etc/xdg/xfce4 ]; then
    sudo cp $SCRIPTS/dot_files/$DEFAULT_KEYMAPFILE /etc/xdg/xfce4/xmodmaprc
    test -f ~/etc/config.local/dot_xmodmaprc && sudo cp $OPTIONS ~/etc/config.local/dot_xmodmaprc /etc/xdg/xfce4/xmodmaprc
    sudo vim /etc/xdg/xfce4/xmodmaprc /etc/xdg/xfce4/xinitrc
fi

test -f ~/etc/config.local/dot_gitconfig && cp $OPTIONS ~/etc/config.local/dot_gitconfig ~/.gitconfig
vim ~/.gitconfig

test -f ~/.tmp/.gitconfig.bak && rm ~/.tmp/.gitconfig.bak
test -f ~/.viminfo && sudo chown $USER ~/.viminfo

