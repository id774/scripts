#!/bin/sh
#
########################################################################
# Install ClamAV
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/15,2008
#       Stable.
########################################################################

test -d install_clamav || mkdir install_clamav
cd install_clamav
svn co http://svn.clamav.net/svn/clamav-devel/trunk/ clamav
cd clamav
./configure
make
sudo make install
sudo vim /usr/local/etc/freshclam.conf /usr/local/etc/clamd.conf
sudo chmod 700 /usr/local/etc/freshclam.conf
sudo cp /usr/local/etc/freshclam.conf /usr/local/etc/freshclam.conf.base
sudo cp /usr/local/etc/clamd.conf /usr/local/etc/clamd.conf.base
sudo freshclam
cd ..
test -d /usr/local/src/security || sudo mkdir -p /usr/local/src/security
sudo cp -a clamav /usr/local/src/security
rm -rf clamav/
cd ..
rm -rf install_clamav/
