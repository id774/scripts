#!/bin/sh
#
########################################################################
# ClamAV Auto Upgrade Script
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 11/10,2007
#       Add clamscan. Not delete source and object code.
#  v0.1 10/16,2007
#       New.
########################################################################
# Install:
# 1. sudo aptitude install clamav
# 2. install_clamav.sh
# 3. Install this script to cron.
########################################################################

test -d /usr/local/src/security || mkdir -p /usr/local/src/security
#test -d /usr/local/src/security/clamav && rm -rf /usr/local/src/security/clamav
cd /usr/local/src/security
svn co http://svn.clamav.net/svn/clamav-devel/trunk/ clamav
cd clamav
make clean
./configure
make
make install
test -f /usr/local/etc/freshclam.conf.base && cp /usr/local/etc/freshclam.conf.base /usr/local/etc/freshclam.conf
test -f /usr/local/etc/clamd.conf.base && cp /usr/local/etc/clamd.conf.base /usr/local/etc/clamd.conf
chmod 700 /usr/local/etc/freshclam.conf.base
chmod 700 /usr/local/etc/freshclam.conf
freshclam
clamscan / -r -i -l /var/log/clamscan.log

