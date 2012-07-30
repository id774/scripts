#!/bin/sh
#
########################################################################
# Install Hive
#  $1 = version
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 7/30,2012
#       First.
########################################################################

setup_environment() {
    test -n "$1" || VERSION=0.9.0
    test -n "$1" && VERSION=$1
    HIVE=hive-$VERSION
    case $OSTYPE in
      *darwin*)
        OWNER=root:wheel
        OPTIONS=-pR
        ;;
      *)
        OWNER=root:root
        OPTIONS=-a
        ;;
    esac
}

install_hive() {
    setup_environment $*
    mkdir install_hive
    cd install_hive
    wget http://ftp.tsukuba.wide.ad.jp/software/apache/hive/$HIVE/$HIVE.tar.gz
    tar xzvf $HIVE.tar.gz
    rm $HIVE.tar.gz
    test -d /opt/hive/$VERSION && sudo rm -rf /opt/hive/$VERSION
    test -d /opt/hive || sudo mkdir -p /opt/hive
    sudo cp $OPTIONS $HIVE /opt/hive/$VERSION
    sudo chown -R $OWNER /opt/hive/$VERSION
    cd ..
    rm -rf install_hive
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_hive $*
