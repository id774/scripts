#!/bin/sh
#
########################################################################
# Install ClamAV
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 10/22,2009
#       Upgrade repository svn to git.
#  v1.0 8/15,2008
#       Stable.
########################################################################

test -d install_clamav || mkdir install_clamav
cd install_clamav
git clone git://git.clamav.net/git/clamav-devel
cd clamav-devel
./configure
make
sudo make install
sudo vim /usr/local/etc/freshclam.conf /usr/local/etc/clamd.conf
sudo chmod 700 /usr/local/etc/freshclam.conf
sudo cp /usr/local/etc/freshclam.conf /usr/local/etc/freshclam.conf.base
sudo cp /usr/local/etc/clamd.conf /usr/local/etc/clamd.conf.base
sudo freshclam
cd ..
test -d /usr/local/src/security/clamav && sudo rm -rf /usr/local/src/security/clamav
test -d /usr/local/src/security/clamav-devel && sudo rm -rf /usr/local/src/security/clamav-devel
test -d /usr/local/src/security || sudo mkdir -p /usr/local/src/security
case $OSTYPE in
  *darwin*)
    sudo cp -R clamav-devel /usr/local/src/security
    sudo chown -R root:wheel /usr/local/src/security/clamav-devel
    sudo chown root:wheel /usr/local/src/security
    sudo chown root:wheel /usr/local/src
    ;;
  *)
    sudo cp -a clamav-devel /usr/local/src/security
    sudo chown -R root:root /usr/local/src/security/clamav-devel
    sudo chown root:root /usr/local/src/security
    sudo chown root:root /usr/local/src
    ;;
esac
rm -rf clamav-devel/
cd ..
rm -rf install_clamav/
