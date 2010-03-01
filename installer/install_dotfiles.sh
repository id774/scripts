#!/bin/sh

test -n "$1" && DEFAULT_KEYMAPFILE=$1
test -n "$1" || DEFAULT_KEYMAPFILE=dot_xmodmaprc_hhkprojp

case $OSTYPE in
  *darwin*)
    OPTIONS=-v
    ;;
  *)
    OPTIONS=-vd
    ;;
esac

for DOT_FILES in zshrc screenrc vimrc gvimrc gitconfig gitignore
do
  test -d /var/root            && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /var/root/.$DOT_FILES
  test -d /Users/mac           && cp      $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /Users/mac/.$DOT_FILES
  test -d /Users/apple         && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /Users/apple/.$DOT_FILES
  test -d /Users/demo          && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /Users/demo/.$DOT_FILES
  test -d /Users/work          && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /Users/work/.$DOT_FILES
  test -d /root                && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /root/.$DOT_FILES
  test -d /home/ubuntu         && cp      $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /home/ubuntu/.$DOT_FILES
  test -d /home/debian         && cp      $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /home/debian/.$DOT_FILES
  test -d /home/centos         && cp      $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /home/centos/.$DOT_FILES
  test -d /etc/skel            && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /etc/skel/.$DOT_FILES
  test -d /home/plagger        && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /home/plagger/.$DOT_FILES
  test -d /home/testuser       && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /home/testuser/.$DOT_FILES
  test -d /var/lib/postgresql  && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /var/lib/postgresql/.$DOT_FILES
  test -d /export/home/solaris && cp      $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES /export/home/solaris/.$DOT_FILES
done

test -f ~/.zshrc.zwc && rm -f ~/.zshrc.zwc
cd
zsh -c 'zcompile ~/.zshrc'

test -d /etc/xdg/xfce4 && sudo cp $SCRIPTS/dot_files/$DEFAULT_KEYMAPFILE /etc/xdg/xfce4/xmodmaprc
test -d /etc/xdg/xfce4 && test -f ~/etc/config.local/dot_xmodmaprc && sudo cp $OPTIONS ~/etc/config.local/dot_xmodmaprc /etc/xdg/xfce4/xmodmaprc
test -d /etc/xdg/xfce4 && sudo vim /etc/xdg/xfce4/xmodmaprc /etc/xdg/xfce4/xinitrc
test -f ~/etc/config.local/dot_gitconfig && cp $OPTIONS ~/etc/config.local/dot_gitconfig ~/.gitconfig
vim ~/.gitconfig
test -f ~/.tmp/.gitconfig.bak && rm ~/.tmp/.gitconfig.bak
test -f ~/.viminfo && sudo chown $USER ~/.viminfo

