#!/bin/sh
#
########################################################################
# Batch Installers for Debian Desktop
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 5/7,2012
#       Renew for Ubuntu Precise.
#  v0.1 9/28,2011
#       First version.
########################################################################

sudo vim /etc/lightdm/lightdm.conf
# allow-guest=false

sudo vim /etc/resolvconf/resolv.conf.d/base /etc/resolv.conf

sudo ln -fs /var/log/syslog /var/log/messages
