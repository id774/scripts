#!/bin/sh
#
########################################################################
# Batch Installers for Debian Desktop
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 2012-05-07
#       Renew for Ubuntu Precise.
#  v0.1 2011-09-28
#       First version.
########################################################################

sudo vim /etc/lightdm/lightdm.conf
# allow-guest=false

sudo restart lightdm
