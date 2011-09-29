#!/bin/sh
#
########################################################################
# Bulk Apt Install Script for Debian
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 9/28,2011
#       Cut off desktop suite.
#  v0.1 6/16,2011
#       Forked from Initial Setup Script.
########################################################################

export SCRIPTS=$HOME/scripts

# Upgrade
sudo apt-get update && sudo apt-get -y upgrade && sudo apt-get autoclean && sudo apt-get -y autoremove

# Vim
sudo apt-get -y install vim

# Http
sudo apt-get -y install w3m
sudo apt-get -y install lynx
sudo apt-get -y install wget
#sudo apt-get -y install curl
#sudo apt-get -y install ncftp

# SSH
sudo apt-get -y install openssh-server ssh

# Compiler
sudo apt-get -y install build-essential
sudo apt-get -y install gcc g++ g77

# Archiver
sudo apt-get -y install tar zip gzip unzip bzip2
sudo apt-get -y install p7zip p7zip-full
#sudo apt-get -y install p7zip-rar # non-free
#sudo apt-get -y install lha # non-free

# zsh/screen
sudo apt-get -y install zsh
sudo apt-get -y install screen

# System
sudo apt-get -y install rsyslog
sudo apt-get -y install sysvconfig
sudo apt-get -y install ntp
sudo apt-get -y install keychain
sudo apt-get -y install locales
sudo apt-get -y install nkf
sudo apt-get -y install mailx
sudo apt-get -y install xdelta
sudo apt-get -y install anacron
#sudo apt-get -y install linux-source
#sudo apt-get -y install checkinstall
#sudo apt-get -y install alien
sudo apt-get -y install uim

# Programming
sudo apt-get -y install nasm
sudo apt-get -y install gauche
sudo apt-get -y install clisp
sudo apt-get -y install scheme48 cmuscheme48-el
sudo apt-get -y install ghc
sudo apt-get -y install global

# SCM
sudo apt-get -y install subversion
sudo apt-get -y install git-core git-all
#sudo apt-get -y install svk

# Debian Developer Tools
sudo apt-get -y install dpkg-dev lintian debhelper yada equivs cvs-buildpackage dupload fakeroot devscripts debget
sudo apt-get -y install apt-listchanges apt-listbugs

# Exim4
sudo apt-get -y install exim4

# Editor
sudo apt-get -y install texinfo
sudo apt-get -y install emacs23 emacs23-el
sudo apt-get -y install mew stunnel ca-certificates
sudo apt-get -y install w3m-el-snapshot w3m-img imagemagick
sudo apt-get -y install vim-gui-common vim-runtime colordiff
sudo apt-get -y install ctags

# sshfs
sudo apt-get -y install sshfs

# Samba (Not Recommended)
#sudo apt-get -y install samba smbfs smbclient swat

# SQLite
#sudo apt-get -y install sqlite
sudo apt-get -y install sqlite3

# Optional Libraries
sudo apt-get -y install migemo
sudo apt-get -y install gnuserv
sudo apt-get -y install mingw32 mingw32-binutils mingw32-runtime
sudo apt-get -y install libxml2 libxml2-dev
sudo apt-get -y install libxslt1-dev libxml-dev
sudo apt-get -y install libxslt-ruby python-libxslt1
sudo apt-get -y install expat libexpat-dev
sudo apt-get -y install libssl-dev libio-socket-ssl-perl libnet-ssleay-perl
sudo apt-get -y install libtemplate-perl libxml-libxml-perl

# exiftool
sudo apt-get -y install exiftool libimage-exiftool-perl jhead

# KVM
#if [ `egrep '^flags.*(vmx|svm)' /proc/cpuinfo | wc -l` != 0 ]; then
#    sudo apt-get -y install kvm libvirt-bin
#    sudo apt-get -y install python-libvirt
#    #sudo apt-get -y install virt-manager
#    sudo apt-get -y Install kqemu-source qemu
#fi

# Security (Anti-Virus)
sudo apt-get -y install clamav

# manpages
sudo apt-get -y install manpages-ja
sudo apt-get -y install manpages-ja-dev
sudo apt-get -y install xmanpages-ja

# sysstat
sudo apt-get -y install sysstat

# hddtemp
sudo apt-get -y install lm-sensors
sudo apt-get -y install hddtemp

# smartmontools
sudo apt-get -y install smartmontools

# Ruby
sudo apt-get -y install autoconf byacc bison automake
#sudo apt-get -y install autoconf-doc # non-free
sudo apt-get -y install libopenssl-ruby libreadline-dev ruby
sudo apt-get -y install ruby1.8 ruby1.8-dev rubygems rubygems1.8

# Apache
#sudo apt-get -y install apache2
#sudo apt-get -y install apache2-mpm-prefork
#sudo apt-get -y install apache-perl

# Apache Utility
#sudo apt-get -y install apache2-utils

# Java JDK
sudo apt-get -y install openjdk-6-jdk
#sudo apt-get -y install sun-java6-jdk

# Linux kernel source, headers, kbuild (Debian)
#sudo apt-get -y install linux-kbuild-2.6.32 linux-headers linux-source

