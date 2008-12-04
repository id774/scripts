#!/bin/sh
#
########################################################################
# Install paco
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 12/4,2008
#       Stable.
########################################################################

PACO=paco-2.0.6

sudo aptitude install libgtkmm-2.4-dev
mkdir paco
cd paco
wget http://downloads.sourceforge.net/paco/$PACO.tar.gz
tar xzvf $PACO.tar.gz
cd $PACO
./configure --disable-gpaco
make
sudo make install
sudo make logme
cd ../..
rm -rf paco
