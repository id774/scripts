#!/bin/zsh
#
########################################################################
# Initial Setup Script for RHEL
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.3 7/11,2011
#       Packed off ruby and frameworks.
#  v1.2 6/8,2011
#       Update to RHEL 6.
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
test -b /dev/mapper/lv_`/bin/hostname`-root && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname`-root
test -b /dev/mapper/lv_`/bin/hostname`-tmp  && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname`-tmp
test -b /dev/mapper/lv_`/bin/hostname`-var  && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname`-var
test -b /dev/mapper/lv_`/bin/hostname`-opt  && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname`-opt
test -b /dev/mapper/lv_`/bin/hostname`-usr  && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname`-usr
test -b /dev/mapper/lv_`/bin/hostname`-home && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname`-home
test -b /dev/mapper/lv_`/bin/hostname`-LogVol01 && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname`-LogVol01
test -b /dev/mapper/lv_`/bin/hostname`-LogVol02 && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname`-LogVol02
test -b /dev/mapper/lv_`/bin/hostname`-LogVol03 && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname`-LogVol03
test -b /dev/mapper/lv_`/bin/hostname`-LogVol04 && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname`-LogVol04
test -b /dev/mapper/lv_`/bin/hostname`-LogVol05 && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname`-LogVol05
test -b /dev/mapper/lv_`/bin/hostname`-LogVol06 && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname`-LogVol06
test -b /dev/mapper/lv_`/bin/hostname`-LogVol07 && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname`-LogVol07
test -b /dev/mapper/lv_`/bin/hostname`-LogVol08 && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname`-LogVol08
test -b /dev/mapper/lv_`/bin/hostname`-LogVol09 && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname`-LogVol09

# yum packages
$SCRIPTS/installer/rhel_yum.sh

# Vim (Original Build)
#$SCRIPTS/installer/install_ncurses.sh
#vim $SCRIPTS/installer/install_vim.sh
#$SCRIPTS/installer/install_vim.sh

# dot_vim
$SCRIPTS/installer/install_dotvim.sh

# dot_zsh
test -d ~/local/github || mkdir -p ~/local/github
cd ~/local/github
git clone git://github.com/id774/dot_zsh.git
cd
ln -s ~/local/github/dot_zsh
~/local/github/dot_zsh/install_dotzsh.sh

# dot_emacs
test -d ~/local/github || mkdir -p ~/local/github
cd ~/local/github
git clone git://github.com/id774/dot_emacs.git
cd
ln -s ~/local/github/dot_emacs
~/local/github/dot_emacs/install_dotemacs.sh
#$SCRIPTS/installer/install_mew.sh /opt/emacs/23.3/bin/emacs

# dot_files
$SCRIPTS/installer/install_dotfiles.sh

# SQLite3
#$SCRIPTS/installer/install_sqlite.sh

# Ruby
#$SCRIPTS/installer/install_ruby_and_rails.sh

# Samba
#wget http://bookmark.at-ninja.jp/bookmark/smb.conf
#sudo cp smb.conf /etc/samba/smb.conf
#rm smb.conf
#sudo smbpasswd -a $USER

# Share Documents
#wget http://big.freett.com/railsinstall2/share-documents.tar.gz
#sudo tar xzvf share-documents.tar.gz -C /usr/local/share
#rm share-documents.tar.gz
#sudo chmod -R 755 /usr/local/share/share-documents
#ln -s /usr/local/share/share-documents ~/share

# Crypt
$SCRIPTS/installer/install_des.sh
$SCRIPTS/installer/install_crypt.sh src 7.0a
$SCRIPTS/installer/install_crypt.sh win 7.0a
$SCRIPTS/installer/install_crypt.sh mac 7.0a
$SCRIPTS/installer/install_crypt.sh linux-i386 7.0a
#$SCRIPTS/installer/install_crypt.sh linux-amd64 7.0a

# sysadmin scripts
$SCRIPTS/installer/setup_sysadmin_scripts.sh

# web page
test -d ~/local/github || mkdir -p ~/local/github
cd ~/local/github
git clone git://github.com/id774/intraweb-template.git
cd
ln -s ~/local/github/intraweb-template
~/local/github/intraweb-template/install_intraweb.sh

# rc.local
$SCRIPTS/installer/install_rclocal.sh

test -r /etc/modprobe.d/blacklist && sudo vim /etc/modprobe.d/blacklist
test -r /etc/modprobe.d/blacklist.conf && sudo vim /etc/modprobe.d/blacklist.conf

# udev patches
$SCRIPTS/dev/fix_udev_persistent-rules.sh

# Change default
sudo vim /etc/profile
sudo vim /etc/crontab
sudo vim /etc/anacrontab
sudo vim /etc/pam.d/su
sudo vim /etc/ssh/sshd_config
sudo vim /etc/pam.d/sshd
sudo vim /etc/pam.d/login
sudo vim /etc/fstab
sudo vim /etc/hosts

# Upgrade
sudo yum update

# Permissions for /src
sudo chown -R root:root /usr/src
sudo chown -R root:root /usr/local/src

# passwd and group
sudo vim /etc/passwd
sudo vim /etc/group

# Activate root
sudo passwd root

# Erase history
test -f ~/.bash_history && sudo rm ~/.bash_history
test -f ~/.mysql_history && sudo rm ~/.mysql_history
test -f ~/.viminfo && sudo rm ~/.viminfo

# sudoers
sudo vim /etc/sudoers $SCRIPTS/etc/sudoers

