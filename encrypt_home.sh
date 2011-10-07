#!/bin/sh
#
########################################################################
# Encrypt home directory by cryptmount
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/15,2008
#       Stable.
########################################################################
# Note:
# 1. This script depend on cryptmount, install it first.
# 2. Customize this script (add directory etc...)
# 3. Next, run this scripts, For encrypt your home directory.
########################################################################

test -f $HOME/local/`/bin/hostname`.tc || exit 1
test -L $HOME/.tmp && exit 1
test -d $HOME/crypt || exit 1

sudo test -d /home/backup && sudo rm -rf /home/backup
sudo mkdir /home/backup
sudo chmod 750 /home/backup
sudo chown -R root:admin /home/backup

mv $HOME/.tmp          $HOME/crypt/
ln -s $HOME/crypt/.tmp          $HOME/.tmp
mv $HOME/.screen       $HOME/crypt/
ln -s $HOME/crypt/.screen       $HOME/.screen
mv $HOME/tmp           $HOME/crypt/
ln -s $HOME/crypt/tmp           $HOME/tmp
mv $HOME/etc           $HOME/crypt/
ln -s $HOME/crypt/etc           $HOME/etc
mv $HOME/bin           $HOME/crypt/
ln -s $HOME/crypt/bin           $HOME/bin
mv $HOME/arc           $HOME/crypt/
ln -s $HOME/crypt/arc           $HOME/arc
mv $HOME/var           $HOME/crypt/
ln -s $HOME/crypt/var           $HOME/var

test -d $HOME/.thunderbird || exit 0
# Thunderbird
mv $HOME/.thunderbird  $HOME/crypt/.thunderbird
ln -s $HOME/crypt/.thunderbird  $HOME/.thunderbird
# Firefox
mv $HOME/.mozilla      $HOME/crypt/.mozilla
ln -s $HOME/crypt/.mozilla      $HOME/.mozilla
# Skype
mv $HOME/.Skype        $HOME/crypt/
ln -s $HOME/crypt/.Skype        $HOME/.Skype
# MSN Messenger
mv $HOME/.amsn         $HOME/crypt/
ln -s $HOME/crypt/.amsn         $HOME/.amsn
mv $HOME/amsn_received $HOME/crypt/
ln -s $HOME/crypt/amsn_received $HOME/amsn_received
# IRC
mv $HOME/.purple       $HOME/crypt/
ln -s $HOME/crypt/.purple       $HOME/.purple
# 2ch
mv $HOME/.jd           $HOME/crypt/.jd
ln -s $HOME/crypt/.jd           $HOME/.jd
mv $HOME/.ochusha      $HOME/crypt/.ochusha
ln -s $HOME/crypt/.ochusha      $HOME/.ochusha

