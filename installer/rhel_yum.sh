#!/bin/sh
#
########################################################################
# Yum Install Script for RHEL
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.5 9/26,2011
#       Whitelist.
#  v0.4 6/8,2011
#       Update to RHEL 6.
#  v0.3 8/29,2010
#       Various packages added.
#  v0.2 9/19,2007
#       Add packages.
#  v0.1 8/31,2007
#       First version.
########################################################################

sudo yum -y install yum-fastestmirror
sudo yum -y groupinstall "Development Tools"
sudo yum -y install kernel-devel kernel-headers
sudo yum -y install rsync
sudo yum -y install git git-all
sudo yum -y install openssh-server
sudo yum -y install openssh-client
sudo yum -y install ssh
sudo yum -y install gcc g++ g77 gcc-c++
sudo yum -y install gdb cgdb
sudo yum -y install bison
sudo yum -y install tar zip gzip unzip bzip2
sudo yum -y install lha-sjis
sudo yum -y install build-essential
sudo yum -y install patch
sudo yum -y install nasm
sudo yum -y install zsh
sudo yum -y install screen

sudo yum -y install yum-utils
sudo yum -y install rpm-build
sudo yum -y install libtool
sudo yum -y install gettext
sudo yum -y install gtk2-devel
sudo yum -y install dbus-devel
sudo yum -y install librsvg2-devel
sudo yum -y install libwnck-devel
sudo yum -y install policycoreutils-python
sudo yum -y install expect

sudo yum -y install autoconf
sudo yum -y install automake
sudo yum -y install wget
sudo yum -y install curl-devel
sudo yum -y install zlib-devel
sudo yum -y install fuse fuse-devel
sudo yum -y install httpd
sudo yum -y install mod_ssl
sudo yum -y install sqlite sqlite-devel
sudo yum -y install php
sudo yum -y install php-mbstring
sudo yum -y install php-mysql
sudo yum -y install ruby
sudo yum -y install ruby-libs ruby-rdoc ruby-ri ruby-irb ruby-devel ruby-cairo-devel
sudo yum -y install rubygem-nokogiri
sudo yum -y install libxml2 libxml2-devel libxslt libxslt-devel
sudo yum -y install bzip2-devel bzip2-libs bzip2
sudo yum -y install blas-devel lapack-devel
sudo yum -y install openblas-devel
sudo yum -y install openssl openssl-devel
sudo yum -y install readline readline-devel
sudo yum -y install shunit2
sudo yum -y install ntp
sudo yum -y install iperf

sudo yum -y install sysstat
sudo yum -y install dstat

sudo yum -y install ctags ctags-etags
sudo yum -y install vim-enhanced
sudo yum -y install atop fortune-all
sudo yum -y install emacs* anthy-el
sudo yum -y install nkf
sudo yum -y install w3m
sudo yum -y install lynx
sudo yum -y install scons
sudo yum -y install httpd-devel
sudo yum -y install enca
sudo yum -y install apr-devel
sudo yum -y install apr-util-devel
sudo yum -y install graphviz graphviz-devel

sudo yum -y install nmap
sudo yum -y install chkrootkit
sudo yum -y install clamav

#sudo yum -y install mysql mysql-server mysql-devel
#sudo yum -y install subversion mod_dav_svn
#sudo yum -y install libpng-devel
#sudo yum -y install libXcomposite-devel
#sudo yum -y install xorg-x11-proto-devel
#sudo yum -y install libXfixes-devel
#sudo yum -y install libXext-devel
#sudo yum -y install libXdamage-devel
#sudo yum -y install libXrandr-devel
#sudo yum -y install libXrender-devel
#sudo yum -y install startup-notification-devel
#sudo yum -y install libXinerama-devel
#sudo yum -y install libICE-devel
#sudo yum -y install libSM-devel
#sudo yum -y install glib2-devel
#sudo yum -y install imake

#sudo yum -y install yumex gconf-editor hwbrowser
#sudo yum -y install wireshark
#sudo yum -y install nautilus-open-terminal
#sudo yum -y install samba samba-client samba-swat
#sudo yum -y install kdebase kdeartwork kdegraphics kdemultimedia
#sudo yum -y install gcc* compat-gcc* compat-glibc* compat-lib*
#sudo yum -y install lapack blas units [octave atlas]
#sudo yum -y install gnuplot* [grace plplot]
#sudo yum -y install xfig [inkscape]
#sudo yum -y install [maxima*] texmacs
#sudo yum -y install mplayer* vlc gstreamer* xmms xmms-mp3 [ogle*] [amarok]
#sudo yum -y --enablerepo=atrpms install [w32codec] divx4linux
#sudo yum -y --enablerepo=flash install flash-plugin
#sudo yum -y install [xscreensaver*]
#sudo yum -y install bittorrent*
#sudo yum -y install cabextract p7zip p7zip-plugins
#sudo yum -y install thunderbird
#sudo yum -y install gnubiff
#sudo yum -y install [gnomebaker]
#sudo yum -y install k3b

