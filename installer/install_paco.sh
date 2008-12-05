#!/bin/sh
#
########################################################################
# Install paco
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 12/15,2008
#       Keep sources.
#  v1.0 12/4,2008
#       Stable.
########################################################################

PACO=paco-2.0.6

mkdir paco
cd paco

wget http://downloads.sourceforge.net/paco/$PACO.tar.gz
tar xzvf $PACO.tar.gz
cd $PACO
./configure --disable-gpaco
make
sudo make install
sudo make logme
cd ..
sudo mkdir -p /usr/local/src/paco
sudo cp -av $PACO /usr/local/src/paco/
sudo chown -R root:root /usr/local/src/paco

cd ..
rm -rf paco
