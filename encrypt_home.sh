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

test -f ~/local/`/bin/hostname`.tc || exit 1
test -L ~/.tmp && exit 1
test -d ~/crypt || exit 1

sudo test -d /home/backup && sudo rm -rf /home/backup
sudo mkdir /home/backup
sudo chmod 750 /home/backup
sudo chown -R root:admin /home/backup

mv ~/.tmp          ~/crypt/
ln -s ~/crypt/.tmp          ~/.tmp
mv ~/.screen       ~/crypt/
ln -s ~/crypt/.screen       ~/.screen
mv ~/tmp           ~/crypt/
ln -s ~/crypt/tmp           ~/tmp
mv ~/etc           ~/crypt/
ln -s ~/crypt/etc           ~/etc
mv ~/bin           ~/crypt/
ln -s ~/crypt/bin           ~/bin
mv ~/arc           ~/crypt/
ln -s ~/crypt/arc           ~/arc
mv ~/var           ~/crypt/
ln -s ~/crypt/var           ~/var

test -d ~/.thunderbird || exit 0
# Thunderbird
mv ~/.thunderbird  ~/crypt/.thunderbird
ln -s ~/crypt/.thunderbird  ~/.thunderbird
# Firefox
mv ~/.mozilla      ~/crypt/.mozilla
ln -s ~/crypt/.mozilla      ~/.mozilla
# Skype
mv ~/.Skype        ~/crypt/
ln -s ~/crypt/.Skype        ~/.Skype
# MSN Messenger
mv ~/.amsn         ~/crypt/
ln -s ~/crypt/.amsn         ~/.amsn
mv ~/amsn_received ~/crypt/
ln -s ~/crypt/amsn_received ~/amsn_received
# IRC
mv ~/.purple       ~/crypt/
ln -s ~/crypt/.purple       ~/.purple
# 2ch
mv ~/.jd           ~/crypt/.jd
ln -s ~/crypt/.jd           ~/.jd
mv ~/.ochusha      ~/crypt/.ochusha
ln -s ~/crypt/.ochusha      ~/.ochusha

