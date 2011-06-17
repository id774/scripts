#!/bin/sh
#
########################################################################
# Initial Setup Script for Ubuntu/Debian
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v3.0 6/16,2011
#       Fork from Initial Setup Script, cut off debian apt.
#  v2.5 5/24,2011
#       For zsh framework.
#  v2.4 3/28,2011
#       Remove building ruby 1.9 from default.
#  v2.3 2/28,2011
#       Switch aptitude to apt-get.
#  v2.2 8/30,2010
#       Add KVM.
#  v2.1 8/7,2010
#       Update to truecrypt 7.
#  v2.0 7/21,2010
#       Correspond to Lucid.
# v1.27 6/29,2010
#       Update to ruby 1.8.7-p299.
# v1.26 6/7,2010
#       Add sysvconfig.
# v1.25 5/7,2010
#       Update to ruby 1.9 as default.
# v1.24 3/3,2010
#       Add Debian Developer Tools.
# v1.23 2/17,2010
#       Update to ruby 1.8.7-p249, 1.9.1-p378.
#       Add python install.
# v1.22 2/2,2010
#       Add sysklogd.
# v1.21 12/26,2009
#       Update to ruby 1.8.7-p248, 1.9.1-p376.
# v1.20 12/17,2009
#       Add xfwm4-themes.
# v1.19 10/5,2009
#       Add exiftool.
# v1.18 8/27,2009
#       Update to rails 2.3.3.
# v1.17 7/31,2009
#       Remove opera.
# v1.16 5/18,2009
#       Remove uim-el, and update misc setup.
# v1.15 5/13,2009
#       Add ubuntu-ja, OpenOffice.org, codec, icons.
#       Setting reserved blocks percentage of ext3 filesystem to 1%.
# v1.14 4/29,2009
#       Add GNU GLOBAL.
# v1.13 3/11,2009
#       Disable local build vim.
# v1.12 1/19,2009
#       Add monitoring tools.
# v1.11 1/18,2009
#       Update to truecrypt 6.1a.
# v1.10 12/31,2008
#       Add emacs-w3m.
#  v1.9 12/26,2008
#       Add paco.
#  v1.8 12/11,2008
#       Add Bitstream Vera Sans Mono, set emacs default, purge vim-gnome.
#  v1.7 11/06,2008
#       Change root shell from zsh to bash.
#  v1.6 10/30,2008
#       Emacs snapshot as default.
#  v1.5 10/24,2008
#       Add sysstat.
#  v1.4 10/14,2008
#       Splash various problems of initial setup and set debian as default.
#  v1.3 9/25,2008
#       Auto tune2fs when using LVM.
#  v1.2 9/23,2008
#       Add curl.
#  v1.1 8/14,2008
#       Automatic Install for gpg,browser,iptable,trac,passenger,
#       and Install Xfce4.
#  v1.0 5/12,2008
#       Stable, for Ubuntu Hardy.
#  v0.6 4/17,2008
#       Remove samba, adding sshfs.
#  v0.5 1/28,2008
#       Last setup added.
#  v0.4 11/23,2007
#       PostgreSQL added.
#  v0.3 10/16,2007
#       Save source code.
#  v0.2 9/4,2007
#       Add Optional Application Install.
#  v0.1 8/27,2007
#       First version.
########################################################################

export SCRIPTS=$HOME/scripts

# Spec
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

# Admin Groups
sudo groupadd admin
sudo groupadd wheel

# APT Update
DISTRIB_CODENAME=squeeze
test -f /etc/lsb-release && DISTRIB_CODENAME=lucid
SOURCESLIST=sources-$DISTRIB_CODENAME.list
sudo cp $SCRIPTS/etc/$SOURCESLIST /etc/apt/sources.list
sudo vim /etc/apt/sources.list
sudo apt-get update

# Ubuntu-ja GPG
#eval `cat /etc/lsb-release`
#wget -q http://www.ubuntulinux.jp/ubuntu-ja-archive-keyring.gpg -O- | sudo apt-key add -
#sudo wget http://www.ubuntulinux.jp/sources.list.d/$DISTRIB_CODENAME.list -O /etc/apt/sources.list.d/ubuntu-ja.list
#sudo apt-get update

# chromium-daily GPG keys
#sudo apt-key adv --recv-keys --keyserver keyserver.ubuntu.com 0xfbef0d696de1c72ba5a835fe5a9bf3bb4e5e17b5

# apt packages
$SCRIPTS/installer/debian_apt.sh

# Network Settings
$SCRIPTS/installer/install_pppconfig.sh

# Stop Services
sudo update-rc.d -f cupsys remove
sudo update-rc.d -f hplip remove
sudo apt-get remove apt-index-watcher

# Home Permission
sudo mkdir -p /opt/sbin
sudo mkdir -p /opt/bin
sudo chmod 750 /home/*

# zsh/screen
chsh -s /bin/zsh
sudo chsh -s /bin/sh root

# Debian Developer Tools
sudo vim /etc/apt/apt.conf.d/10apt-listbugs*

# Exim4
sudo dpkg-reconfigure exim4-config

# paco
#$SCRIPTS/installer/install_paco.sh

# Editor
#$SCRIPTS/installer/install_emacs.sh 23.3 /opt/emacs/23.3
#$SCRIPTS/installer/install_emacs_w3m.sh 23.3 /opt/emacs/23.3
#sudo ln -fs /opt/emacs/23.3/bin/emacs /opt/bin/emacs

# navi2ch
#$SCRIPTS/installer/install_navi2ch.sh

# Vim
#$SCRIPTS/installer/install_ncurses.sh
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

# sshfs
sudo vim /etc/modules

# Samba (Not Recommended)
#sudo update-rc.d -f samba remove
#sudo cp $SCRIPTS/etc/smb.conf /etc/samba/smb.conf
#sudo smbpasswd -a $USER

# PostgreSQL
$SCRIPTS/installer/install_postgres.py install

# MySQL
$SCRIPTS/installer/install_mysql.py install -c

# KVM
#if [ `egrep '^flags.*(vmx|svm)' /proc/cpuinfo | wc -l` != 0 ]; then
#    sudo addgroup $USER libvirtd
#    sudo addgroup $USER kvm
#fi

# Crypt
$SCRIPTS/installer/install_des.sh
$SCRIPTS/installer/install_crypt.sh src 7.0a
$SCRIPTS/installer/install_crypt.sh win 7.0a
$SCRIPTS/installer/install_crypt.sh mac 7.0a
$SCRIPTS/installer/install_crypt.sh linux-i386 7.0a
#$SCRIPTS/installer/install_crypt.sh linux-amd64 7.0a

# iptables
$SCRIPTS/installer/install_iptables.sh

# sysstat
sudo dpkg-reconfigure sysstat
# ENABLED="true"
sudo vim /etc/default/sysstat

# hddtemp
sudo dpkg-reconfigure hddtemp

# smartmontools
# start_smartd=yes
# smartd_opts="--interval=7200"
sudo vim /etc/default/smartmontools

# Ruby
#$SCRIPTS/installer/install_ruby.sh 187-svn /opt/ruby/1.8.7
#$SCRIPTS/installer/install_ruby.sh 191-svn /opt/ruby/1.9.1
#$SCRIPTS/installer/install_ruby.sh 192-svn /opt/ruby/1.9.2
#$SCRIPTS/config/update-alternatives-ruby.sh

# RubyGems
#$SCRIPTS/installer/install_rubygems.sh 162 /opt/ruby/1.9.2
#$SCRIPTS/installer/install_gems.sh /opt/ruby/1.9.2
#$SCRIPTS/installer/install_rails.sh 300 /opt/ruby/1.9.2
#/opt/ruby/1.9.2/bin/vim-ruby-install.rb

# Passenger
#$SCRIPTS/installer/install_passenger.sh /opt/ruby/1.9.2
#$SCRIPTS/config/update-alternatives-ruby.sh

# Python
#$SCRIPTS/installer/install_python.sh 2.7.1 /opt/python/2.7.1
#$SCRIPTS/installer/install_python.sh 3.1.3 /opt/python/3.1.3
#$SCRIPTS/config/update-alternatives-python.sh

# Python Framework
#vim $SCRIPTS/installer/install_python_framework.sh
#$SCRIPTS/installer/install_python_framework.sh

# Trac
#$SCRIPTS/installer/install_trac.sh

# sysadmin scripts
$SCRIPTS/installer/setup_sysadmin_scripts.sh

# GUI Desktop Xfce4(Debian) / Xubuntu(Ubuntu)
#im-switch -c
#sudo rmmod pcspkr
test -r /etc/modprobe.d/blacklist && sudo vim /etc/modprobe.d/blacklist
test -r /etc/modprobe.d/blacklist.conf && sudo vim /etc/modprobe.d/blacklist.conf
#$SCRIPTS/installer/install_dotfiles.sh dot_xmodmaprc_lucid
# xfsuspend
#which s2ram > /dev/null && which xflock4 > /dev/null && sudo cp $SCRIPTS/xfsuspend.sh /usr/local/sbin/xfsuspend && sudo chown root:root /usr/local/sbin/xfsuspend && sudo chmod 755 /usr/local/sbin/xfsuspend && sudo vim /usr/local/sbin/xfsuspend
# xfce4 custom themes
#test -f /usr/share/themes/Xfce-dusk/gtk-2.0/gtkrc && sudo cp ~/scripts/etc/themes/xfce-dusk/gtkrc /usr/share/themes/Xfce-dusk/gtk-2.0/gtkrc

# GDM Themes
#$SCRIPTS/installer/install_gdmthemes.sh
#$SCRIPTS/installer/install_gdmthemes2.sh

# Share Documents
#wget http://big.freett.com/railsinstall2/share-documents.tar.gz
#sudo tar xzvf share-documents.tar.gz -C /usr/local/share
#rm share-documents.tar.gz
#sudo chmod -R 755 /usr/local/share/share-documents
#ln -s /usr/local/share/share-documents ~/share

# Iceweasel and Icedove (Debian)
#$SCRIPTS/installer/install_iceweasel.sh

# Install plagger plugin
#$SCRIPTS/installer/install_plagger_plugins.sh

# Termtter
cd ~/local/github
git clone git://github.com/id774/termtter-plugins.git
cd
ln -s ~/local/github/termtter-plugins
#$PRIVATE/installer/install_dottermtter.sh
#$SCRIPTS/installer/install_termtter_plugins.sh

# rc.local
$SCRIPTS/installer/install_rclocal.sh

# Permissions for /src
sudo chown -R root:root /usr/src
sudo chown -R root:root /usr/local/src

# ntp server (130.69.251.23)
sudo vim /etc/ntp.conf

# Apache Configuration
test -f /etc/apache2/apache2.conf && sudo vim /etc/apache2/apache2.conf

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
sudo vim /etc/deluser.conf
sudo vim /etc/hosts

# Grub
test -f /boot/grub/menu.lst && sudo vim /boot/grub/menu.lst
test -f /etc/default/grub && sudo vim /etc/default/grub && sudo update-grub2

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

