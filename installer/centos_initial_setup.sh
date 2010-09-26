#!/bin/zsh
#
########################################################################
# Initial Setup Script for CentOS 5
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 8/29,2010
#       Backported from Ubuntu/Debian.
#  v1.0 8/21,2008
#       Stable.
#  v0.3 5/12,2008
#       Backported from Ubuntu.
#  v0.2 9/4,2007
#       Add Optional Application Install.
#  v0.1 8/31,2007
#       First version.
########################################################################

export SCRIPTS=$HOME/scripts

# Show Memory and CPU
cat /proc/meminfo
cat /proc/cpuinfo

# tune2fs
test -b /dev/sda0  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda0
test -b /dev/sda1  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda1
test -b /dev/sda2  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda2
test -b /dev/sda3  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda3
test -b /dev/sda4  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda4
test -b /dev/sda5  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda5
test -b /dev/sda6  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda6
test -b /dev/sda7  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda7
test -b /dev/sda8  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda8
test -b /dev/sda9  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda9
test -b /dev/sda10 && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda10
test -b /dev/mapper/`/bin/hostname`-root && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/`/bin/hostname`-root
test -b /dev/mapper/`/bin/hostname`-tmp  && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/`/bin/hostname`-tmp
test -b /dev/mapper/`/bin/hostname`-var  && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/`/bin/hostname`-var
test -b /dev/mapper/`/bin/hostname`-opt  && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/`/bin/hostname`-opt
test -b /dev/mapper/`/bin/hostname`-usr  && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/`/bin/hostname`-usr
test -b /dev/mapper/`/bin/hostname`-home && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/`/bin/hostname`-home

# Make Directory
mkdir ~/.tmp
mkdir ~/.screen
mkdir ~/tmp
mkdir ~/etc
mkdir ~/bin
mkdir ~/arc
mkdir ~/mnt
mkdir ~/var
mkdir ~/local
chmod 700 ~/.tmp
chmod 700 ~/.screen
chmod 700 ~/tmp
chmod 700 ~/etc
chmod 700 ~/bin
chmod 700 ~/arc
chmod 700 ~/mnt
chmod 700 ~/var
chmod 700 ~/local
sudo mkdir /root/.tmp
sudo mkdir /root/.screen
sudo mkdir /root/tmp
sudo mkdir /root/etc
sudo mkdir /root/bin
sudo mkdir /root/arc
sudo mkdir /root/mnt
sudo mkdir /root/var
sudo mkdir /root/local
sudo chmod 700 /root/.tmp
sudo chmod 700 /root/.screen
sudo chmod 700 /root/tmp
sudo chmod 700 /root/etc
sudo chmod 700 /root/bin
sudo chmod 700 /root/arc
sudo chmod 700 /root/mnt
sudo chmod 700 /root/var
sudo chmod 700 /root/local
sudo chmod 750 /root
sudo mkdir /etc/skel/.tmp
sudo mkdir /etc/skel/.screen
sudo mkdir /etc/skel/tmp
sudo mkdir /etc/skel/etc
sudo mkdir /etc/skel/bin
sudo mkdir /etc/skel/arc
sudo mkdir /etc/skel/mnt
sudo mkdir /etc/skel/var
sudo mkdir /etc/skel/local
sudo chmod 700 /etc/skel/.tmp
sudo chmod 700 /etc/skel/.screen
sudo chmod 700 /etc/skel/tmp
sudo chmod 700 /etc/skel/etc
sudo chmod 700 /etc/skel/bin
sudo chmod 700 /etc/skel/arc
sudo chmod 700 /etc/skel/mnt
sudo chmod 700 /etc/skel/var
sudo chmod 700 /etc/skel/local
sudo chmod 750 /home/*

# yum packages
$SCRIPTS/installer/centos_yum.sh

# Vim (Original Build)
$SCRIPTS/installer/install_ncurses.sh
vim $SCRIPTS/installer/install_vim.sh
$SCRIPTS/installer/install_vim.sh

# Deploy dot_vim
$SCRIPTS/installer/install_dotvim.sh

# SQLite3
$SCRIPTS/installer/install_sqlite.sh

# Ruby
$SCRIPTS/installer/install_ruby.sh 192-svn /opt/ruby/1.9.2

# RubyGems
$SCRIPTS/installer/install_gems.sh /opt/ruby/1.9.2
$SCRIPTS/installer/install_rails.sh rails-ruby /opt/ruby/1.9.2
/opt/ruby/1.9.2/bin/vim-ruby-install.rb

# Deploy dot_files
$SCRIPTS/installer/install_dotfiles.sh

# Crypt
$SCRIPTS/installer/install_des.sh
$SCRIPTS/installer/install_crypt.sh src
$SCRIPTS/installer/install_crypt.sh win
$SCRIPTS/installer/install_crypt.sh mac
$SCRIPTS/installer/install_crypt.sh linux-i386
#$SCRIPTS/installer/install_crypt.sh linux-amd64

# Samba
#wget http://bookmark.at-ninja.jp/bookmark/smb.conf
#sudo cp smb.conf /etc/samba/smb.conf
#rm smb.conf
#sudo smbpasswd -a $USER

# MySQL
#$SCRIPTS/installer/centos_mysql_init.sh

# Share Documents
#wget http://big.freett.com/railsinstall2/share-documents.tar.gz
#sudo tar xzvf share-documents.tar.gz -C /usr/local/share
#rm share-documents.tar.gz
#sudo chmod -R 755 /usr/local/share/share-documents
#ln -s /usr/local/share/share-documents ~/share

# Permissions for /src
sudo chown -R root:root /usr/src
sudo chown -R root:root /usr/local/src

