#!/bin/sh
#
########################################################################
# Yum Install Script for CentOS 5
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 9/19,2007
#       Add packages.
#  v0.1 8/31,2007
#       First version.
########################################################################
# Note:
# 1. First, run this script with root.
# 2. Next, run other scripts.
########################################################################

yum -y install openssh-server
yum -y install ssh
yum -y install gcc g++ g77
yum -y install tar zip gzip unzip bzip2
yum -y install lha-sjis
yum -y install build-essential
yum -y install zsh
yum -y install screen
yum -y install nautilus-open-terminal
yum -y install httpd
yum -y install mysql-server
yum -y install php
yum -y install php-mbstring
yum -y install php-mysql
yum -y install ntp
yum -y install autoconf
yum -y install automake
yum -y install subversion
yum -y install libpng-devel
yum -y install libXcomposite-devel
yum -y install xorg-x11-proto-devel
yum -y install libXfixes-devel
yum -y install libXext-devel
yum -y install libXdamage-devel
yum -y install libXrandr-devel
yum -y install libXrender-devel
yum -y install startup-notification-devel
yum -y install libXinerama-devel
yum -y install libICE-devel
yum -y install libSM-devel
yum -y install glib2-devel
yum -y install imake
yum -y install rpm-build
yum -y install libtool
yum -y install gettext
yum -y install gtk2-devel
yum -y install dbus-devel
yum -y install librsvg2-devel
yum -y install libwnck-devel
yum -y install thunderbird
yum -y install samba samba-client samba-swat
yum -y install vim-X11 vim-runtime
yum -y install wireshark wireshark-gnome
yum -y install yumex yum-utils gconf-editor hwbrowser rpm-build
yum -y install atop fortune-all
yum -y install emacs* anthy-el
yum -y install kdebase kdeartwork kdegraphics kdemultimedia
yum -y install gcc* compat-gcc* compat-glibc* compat-lib*
yum -y install lapack blas units [octave atlas]
yum -y install gnuplot* [grace plplot]
yum -y install xfig [inkscape]
yum -y install [maxima*] texmacs
yum -y install mplayer* vlc gstreamer* xmms xmms-mp3 [ogle*] [amarok]
yum -y --enablerepo=atrpms install [w32codec] divx4linux
yum -y --enablerepo=flash install flash-plugin
yum -y install [xscreensaver*]
yum -y install bittorrent*
yum -y install cabextract p7zip p7zip-plugins
yum -y install thunderbird
yum -y install gnubiff
yum -y install [gnomebaker]
yum -y install k3b

