#!/bin/sh
#
########################################################################
# Install Navi2ch
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 1/19,2009
#       First version.
########################################################################

VER=1.8.1
SOURCE="http://downloads.sourceforge.net/navi2ch/navi2ch-$VER.tar.gz"

mkdir install_navi2ch
cd install_navi2ch

wget $SOURCE
tar xzvf navi2ch-$VER.tar.gz
cd navi2ch-$VER
./configure
make
make check
sudo make install

cd ../..
rm -rf install_navi2ch
