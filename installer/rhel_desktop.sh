#!/bin/sh
#
########################################################################
# Desktop Environment for RHEL
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2011-06-15
#       First version.
########################################################################

sudo yum -y groupinstall "X Window System"
sudo yum -y groupinstall "Desktop"
sudo yum -y groupinstall "General Purpose Desktop"

