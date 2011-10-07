#!/bin/sh
#
########################################################################
# Install Linux Kernel Image
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 10/7,2011
#       First.
########################################################################

install_kernel() {
    VERSION=$1
    KERNEL=$2
    KERNEL_VER=$VERSION-$KERNEL
    IMAGE=linux-image-$KERNEL_VER
    HEADER=linux-headers-$KERNEL_VER
    sudo apt-get install -y $IMAGE $HEADER
}

test -n "$2" || exit 1
ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_kernel $*
