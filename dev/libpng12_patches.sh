#!/bin/sh

wget -q -O $HOME/tmp/libpng12.deb http://mirrors.kernel.org/ubuntu/pool/main/libp/libpng/libpng12-0_1.2.54-1ubuntu1_amd64.deb && sudo dpkg -i $HOME/tmp/libpng12.deb && rm -f $HOME/tmp/libpng12.deb

